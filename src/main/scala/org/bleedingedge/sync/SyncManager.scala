package org.bleedingedge.sync

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.codec.Serialization
import org.bleedingedge.domain.{Command, LocationState, Snapshot}
import org.bleedingedge.infrastructure.scheduling.Scheduler
import org.bleedingedge.network.{NetworkManager, NetworkMessage, PeerInfo}
import org.bleedingedge.resource.{CommandExecutor, FileSystemMonitor}
import org.bleedingedge.transposition.StateTransformer

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Manages synchronization of files across peers.
 *
 * Coordinates between local file system monitoring and network communication
 * to keep file states synchronized across multiple peers.
 *
 * Responsibilities:
 * - Monitor local file system for changes
 * - Broadcast state updates to peers
 * - Process incoming state broadcasts
 * - Handle state requests from peers
 * - Apply remote changes locally
 * - Maintain current snapshot of local state
 *
 * @param basePath The directory to synchronize
 * @param scheduler Scheduler for async operations
 * @since 2.0.0
 */
class SyncManager(
  val basePath: Path,
  scheduler: Scheduler
) extends LazyLogging:

  private val running = AtomicBoolean(false)
  private val currentSnapshot = AtomicReference[Snapshot](Snapshot.empty)
  private val peerSnapshots = ConcurrentHashMap[PeerInfo, Snapshot]()
  private val accumulatedStates = scala.collection.mutable.ListBuffer[LocationState]()
  private var networkManager: Option[NetworkManager] = None
  private var monitorFuture: Option[Future[Unit]] = None
  private val commandExecutor = CommandExecutor(basePath)(using scheduler.executionContext)

  given scala.concurrent.ExecutionContext = scheduler.executionContext

  /**
   * Connects this sync manager to a network manager.
   *
   * The network manager's message callback should route messages to
   * this sync manager's handleMessage method.
   */
  def connect(manager: NetworkManager): Unit =
    networkManager = Some(manager)
    logger.info("Connected to network manager")

  /**
   * Starts the sync manager.
   *
   * Begins monitoring the local file system and processing network messages.
   *
   * @return Future that completes when sync is started
   */
  def start(): Future[Unit] =
    if running.compareAndSet(false, true) then
      logger.info(s"Starting sync manager for $basePath")

      // Initialize current snapshot from file system
      initializeSnapshot()

      // Start file system monitoring
      val monitor = FileSystemMonitor(scheduler)
      val future = monitor.scanDirectoryForChanges(basePath, handleFileSystemChange)
      monitorFuture = Some(future)

      Future.successful(())
    else
      Future.failed(IllegalStateException("Sync manager already running"))

  /**
   * Stops the sync manager.
   */
  def stop(): Unit =
    if running.compareAndSet(true, false) then
      logger.info("Stopping sync manager")

      // File monitoring will stop on its own (it's Future-based)
      monitorFuture = None

      peerSnapshots.clear()

  /**
   * Returns true if the sync manager is running.
   */
  def isRunning: Boolean = running.get()

  /**
   * Returns the current snapshot of local state.
   */
  def localSnapshot: Snapshot = currentSnapshot.get()

  /**
   * Handles a message received from a peer.
   *
   * This is called by NetworkManager when messages arrive.
   */
  def handleMessage(peer: PeerInfo, message: NetworkMessage): Unit =
    if !running.get() then return

    message match
      case NetworkMessage.Hello(peerId, hostname) =>
        handleHello(peer, peerId, hostname)

      case NetworkMessage.StateBroadcast(stateBytes) =>
        handleStateBroadcast(peer, stateBytes)

      case NetworkMessage.StateRequest(hostname, resourceHash, resourceLength) =>
        handleStateRequest(peer, hostname, resourceHash, resourceLength)

      case NetworkMessage.Heartbeat(timestamp) =>
        handleHeartbeat(peer, timestamp)

      case NetworkMessage.Goodbye(peerId) =>
        handleGoodbye(peer, peerId)

      case NetworkMessage.Acknowledgment(messageId) =>
        handleAcknowledgment(peer, messageId)

  /**
   * Initializes the current snapshot from the file system using FileSystemMonitor.
   */
  private def initializeSnapshot(): Unit =
    val monitor = FileSystemMonitor(scheduler)
    monitor.scanDirectory(basePath) match
      case scala.util.Success(states) =>
        val snapshot = if states.isEmpty then
          Snapshot.empty
        else
          StateTransformer.statesToSnapshots(states).lastOption.getOrElse(Snapshot.empty)

        currentSnapshot.set(snapshot)
        logger.info(s"Initialized snapshot with ${states.size} files from $basePath")
        if states.nonEmpty then
          logger.info(s"Files in snapshot: ${states.map(_.location).mkString(", ")}")

      case scala.util.Failure(e) =>
        logger.error("Failed to initialize snapshot", e)
        currentSnapshot.set(Snapshot.empty)

  /**
   * Handles a file system change event.
   */
  private def handleFileSystemChange(state: LocationState): Unit =
    if !running.get() then return

    logger.debug(s"File system changed: ${state.location}")

    // Ignore empty states (race condition with file creation)
    if state.resourceId.originalLength == 0 && state.location.nonEmpty then
      logger.debug(s"Ignoring empty state for ${state.location} (likely race with file creation)")
      return

    // Accumulate state and update snapshot
    accumulatedStates.synchronized {
      accumulatedStates += state

      // Convert states to snapshot and update
      val snapshots = StateTransformer.statesToSnapshots(accumulatedStates.toList)
      snapshots.lastOption.foreach { newSnapshot =>
        val oldSnapshot = currentSnapshot.getAndSet(newSnapshot)

        // Broadcast changes to all peers
        broadcastStateChanges(oldSnapshot, newSnapshot)

        // Clear accumulated states after creating snapshot
        accumulatedStates.clear()
      }
    }

  /**
   * Broadcasts state changes to all connected peers.
   */
  private def broadcastStateChanges(oldSnapshot: Snapshot, newSnapshot: Snapshot): Unit =
    networkManager match
      case Some(manager) =>
        Try {
          // Serialize the new snapshot states
          val statesList = newSnapshot.states
          val stateBytes = Serialization.statesToBytes(statesList)

          // Broadcast to all peers
          val message = NetworkMessage.StateBroadcast(stateBytes)
          manager.broadcast(message).recover {
            case e: Exception =>
              logger.warn(s"Failed to broadcast state changes: ${e.getMessage}")
          }

          logger.debug(s"Broadcasted ${statesList.size} states to peers")
        }.recover {
          case e: Exception =>
            logger.error("Error broadcasting state changes", e)
        }

      case None =>
        logger.warn("Cannot broadcast: not connected to network manager")

  /**
   * Handles a Hello message from a peer.
   */
  private def handleHello(peer: PeerInfo, peerId: java.util.UUID, hostname: String): Unit =
    logger.info(s"Received Hello from peer: $hostname ($peerId)")

    networkManager match
      case Some(manager) =>
        // Send our current snapshot to the new peer after a small delay
        // This ensures both sides are ready to receive messages
        val statesList = currentSnapshot.get().states
        if statesList.nonEmpty then
          scheduler.execute("send-initial-state") {
            Thread.sleep(100)  // Give peer time to start its receive loop

            val stateBytes = Serialization.statesToBytes(statesList)
            val message = NetworkMessage.StateBroadcast(stateBytes)

            manager.sendToPeer(peer.id, message).recover {
              case e: Exception =>
                logger.warn(s"Failed to send initial state to ${peer.displayName}: ${e.getMessage}")
            }
          }

      case None =>
        logger.warn("Cannot send initial state: not connected to network manager")

  /**
   * Handles a StateBroadcast message from a peer.
   */
  private def handleStateBroadcast(peer: PeerInfo, stateBytes: Array[Byte]): Unit =
    Try {
      // Deserialize states
      val states = Serialization.bytesToStates(stateBytes)
      logger.debug(s"Received ${states.size} states from ${peer.displayName}")

      // Convert states to snapshot
      val snapshots = StateTransformer.statesToSnapshots(states)
      snapshots.lastOption.foreach { peerSnapshot =>
        // Store peer's snapshot
        peerSnapshots.put(peer, peerSnapshot)

        // Compute differences with our current snapshot
        val localSnapshot = currentSnapshot.get()
        val commands = StateTransformer.commandsBetween(localSnapshot, peerSnapshot)

        // Apply commands to sync our local state
        applyCommands(commands.toList)
      }
    }.recover {
      case e: Exception =>
        logger.error(s"Error processing state broadcast from ${peer.displayName}", e)
    }

  /**
   * Handles a StateRequest message from a peer.
   */
  private def handleStateRequest(
    peer: PeerInfo,
    hostname: String,
    resourceHash: String,
    resourceLength: Int
  ): Unit =
    logger.debug(s"Received state request from ${peer.displayName}: $resourceHash")

    networkManager match
      case Some(manager) =>
        // Find the requested state in our current snapshot
        currentSnapshot.get().states.find { state =>
          state.resourceId.resourceHash == resourceHash && state.resourceId.originalLength == resourceLength
        } match
          case Some(state) =>
            // Send the requested state
            val stateBytes = Serialization.statesToBytes(List(state))
            val message = NetworkMessage.StateBroadcast(stateBytes)

            manager.sendToPeer(peer.id, message).recover {
              case e: Exception =>
                logger.warn(s"Failed to send state to ${peer.displayName}: ${e.getMessage}")
            }

          case None =>
            logger.warn(s"Requested state not found: $resourceHash")

      case None =>
        logger.warn("Cannot send state: not connected to network manager")

  /**
   * Handles a Heartbeat message from a peer.
   */
  private def handleHeartbeat(peer: PeerInfo, timestamp: Long): Unit =
    logger.trace(s"Received heartbeat from ${peer.displayName}")
    // Could track peer health here

  /**
   * Handles a Goodbye message from a peer.
   */
  private def handleGoodbye(peer: PeerInfo, peerId: java.util.UUID): Unit =
    logger.info(s"Received Goodbye from peer: ${peer.displayName}")
    peerSnapshots.remove(peer)

  /**
   * Handles an Acknowledgment message from a peer.
   */
  private def handleAcknowledgment(peer: PeerInfo, messageId: java.util.UUID): Unit =
    logger.trace(s"Received acknowledgment from ${peer.displayName}: $messageId")
    // Could track message delivery here

  /**
   * Applies a list of commands using CommandExecutor from the Resource layer.
   */
  private def applyCommands(commands: List[Command]): Unit =
    val results = commandExecutor.executeAll(commands)
    results.zip(commands).foreach { case (result, command) =>
      result match
        case scala.util.Success(_) =>
          logger.debug(s"Applied command: ${command.getClass.getSimpleName}")
        case scala.util.Failure(e) =>
          logger.error(s"Failed to apply command: ${command.getClass.getSimpleName}", e)
    }

object SyncManager:

  /**
   * Creates a sync manager.
   *
   * Use the connect() method to attach a network manager after creation.
   */
  def apply(
    basePath: Path,
    scheduler: Scheduler
  ): SyncManager =
    new SyncManager(basePath, scheduler)
