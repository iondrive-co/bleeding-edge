package org.bleedingedge.network

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.infrastructure.scheduling.Scheduler

import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

/**
 * Manages all network connections and coordinates peer-to-peer communication.
 *
 * Responsibilities:
 * - Accept incoming connections
 * - Manage active peer connections
 * - Coordinate peer discovery
 * - Route messages between peers
 * - Handle connection lifecycle
 *
 * @param port The port to listen on for incoming connections
 * @param scheduler Scheduler for async operations
 * @param onMessage Callback when a message is received from any peer
 * @since 2.0.0
 */
class NetworkManager(
  val port: Int,
  scheduler: Scheduler,
  onMessage: (PeerInfo, NetworkMessage) => Unit = (_, _) => ()
) extends LazyLogging:

  private val localPeerId = UUID.randomUUID()
  private val connections = ConcurrentHashMap[UUID, PeerConnection]()
  private val pendingConnections = ConcurrentHashMap[InetSocketAddress, PeerConnection]()  // Waiting for Hello
  private val discoveredPeers = ConcurrentHashMap[UUID, PeerInfo]()
  private val running = AtomicBoolean(false)
  private var serverSocket: Option[ServerSocket] = None
  private var discovery: Option[PeerDiscovery] = None
  private var actualPort: Int = port  // Actual bound port (may differ from requested port if port=0)

  given scala.concurrent.ExecutionContext = scheduler.executionContext

  logger.info(s"NetworkManager initialized with peer ID: $localPeerId")

  /**
   * Returns the actual port the server is listening on.
   * This may differ from the requested port if port=0 (OS-assigned).
   */
  def getActualPort: Int = actualPort

  /**
   * Starts the network manager.
   *
   * Begins accepting connections and starts peer discovery.
   *
   * @param enableDiscovery Whether to enable automatic peer discovery
   * @return Future that completes when network is started
   */
  def start(enableDiscovery: Boolean = true): Future[Unit] =
    if running.compareAndSet(false, true) then
      logger.info(s"Starting network manager on port $port")

      // Start accepting connections first (this sets actualPort)
      startAcceptingConnections().flatMap { _ =>
        // Now start peer discovery with the actual bound port and our peer ID
        if enableDiscovery then
          val disc = PeerDiscovery(localPeerId, actualPort, scheduler, handlePeerDiscovered)
          discovery = Some(disc)
          disc.start()
        else
          Future.successful(())
      }
    else
      Future.failed(IllegalStateException("Network manager already running"))

  /**
   * Stops the network manager.
   *
   * Closes all connections and stops peer discovery.
   */
  def stop(): Unit =
    if running.compareAndSet(true, false) then
      logger.info("Stopping network manager")

      // Stop discovery
      discovery.foreach(_.stop())

      // Disconnect all peers
      connections.values().asScala.foreach(_.disconnect())
      connections.clear()

      // Close server socket
      serverSocket.foreach(s => Try(s.close()))
      serverSocket = None

      discoveredPeers.clear()

  /**
   * Returns true if the network manager is running.
   */
  def isRunning: Boolean = running.get()

  /**
   * Returns all currently connected peers.
   */
  def connectedPeers: List[PeerInfo] =
    connections.values().asScala
      .filter(_.isConnected)
      .map(_.peerInfo)
      .toList

  /**
   * Returns the number of active connections.
   */
  def connectionCount: Int = connectedPeers.size

  /**
   * Connects to a specific peer.
   *
   * @param peerInfo Information about the peer to connect to
   * @return Future completing when connection is established
   */
  def connectToPeer(peerInfo: PeerInfo): Future[Unit] =
    if !running.get() then
      Future.failed(IllegalStateException("Network manager not running"))
    else if connections.containsKey(peerInfo.id) then
      Future.failed(IllegalStateException(s"Already connected to peer: ${peerInfo.displayName}"))
    else
      val connection = PeerConnection(
        localPeerId,
        peerInfo,
        scheduler,
        handleMessage,
        handleDisconnect
      )

      connections.put(peerInfo.id, connection)
      connection.connect().recover {
        case e: Exception =>
          connections.remove(peerInfo.id)
          throw e
      }

  /**
   * Broadcasts a message to all connected peers.
   *
   * @param message The message to broadcast
   * @return Future completing when message is sent to all peers
   */
  def broadcast(message: NetworkMessage): Future[Unit] =
    val sends = connections.values().asScala
      .filter(_.isConnected)
      .map(_.sendMessage(message))
      .toSeq

    Future.sequence(sends).map(_ => ())

  /**
   * Sends a message to a specific peer.
   *
   * @param peerId The ID of the peer to send to
   * @param message The message to send
   * @return Future completing when message is sent
   */
  def sendToPeer(peerId: UUID, message: NetworkMessage): Future[Unit] =
    Option(connections.get(peerId)) match
      case Some(conn) if conn.isConnected =>
        conn.sendMessage(message)
      case Some(_) =>
        Future.failed(IllegalStateException(s"Peer not connected: $peerId"))
      case None =>
        Future.failed(IllegalStateException(s"Unknown peer: $peerId"))

  /**
   * Starts accepting incoming connections.
   */
  private def startAcceptingConnections(): Future[Unit] =
    Try {
      val server = ServerSocket(port)
      server.setReuseAddress(true)
      serverSocket = Some(server)

      // Get actual bound port (important when port=0 for OS-assigned)
      actualPort = server.getLocalPort

      logger.info(s"Listening for connections on port $actualPort" +
        (if port == 0 then " (OS-assigned)" else ""))

      // Start accept loop in background
      scheduler.execute("network-accept") {
        while running.get() do
          try
            val socket = server.accept()
            logger.info(s"Accepted connection from ${socket.getRemoteSocketAddress}")

            // Create connection from accepted socket
            val connection = PeerConnection.fromSocket(
              localPeerId,
              socket,
              scheduler,
              handleMessage,
              handleDisconnect
            )

            // Add to pending connections - will be moved to main map when Hello is received
            val remoteAddress = socket.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]
            pendingConnections.put(remoteAddress, connection)
            logger.debug(s"Added pending connection from $remoteAddress, waiting for Hello message")

          catch
            case _: java.net.SocketException if !running.get() =>
              // Normal shutdown
            case e: Exception =>
              logger.error("Error accepting connection", e)
      }

      // Return immediately after starting accept loop
      ()
    } match
      case Success(_) => Future.successful(())
      case Failure(e) => Future.failed(e)  // Propagate actual exception

  /**
   * Handles discovery of a new peer.
   */
  private def handlePeerDiscovered(peerInfo: PeerInfo): Unit =
    logger.debug(s"handlePeerDiscovered called for: ${peerInfo.displayName} (${peerInfo.id})")

    // Add to discovered peers
    discoveredPeers.put(peerInfo.id, peerInfo)
    logger.debug(s"Added to discovered peers. Total discovered: ${discoveredPeers.size()}")

    // Check if already connected (active or pending)
    val alreadyActive = connections.containsKey(peerInfo.id)
    val alreadyPending = pendingConnections.values().asScala.exists { conn =>
      conn.peerInfo.address == peerInfo.address
    }

    logger.debug(s"Already connected to ${peerInfo.displayName}? active=$alreadyActive, pending=$alreadyPending")
    logger.debug(s"Active connections: ${connections.keySet().asScala.mkString(", ")}")
    logger.debug(s"Pending connections: ${pendingConnections.size()} total")

    if !alreadyActive && !alreadyPending then
      // Tie-breaking: only initiate connection if our peer ID is less than theirs
      // This prevents duplicate bidirectional connections when both peers discover each other
      if localPeerId.compareTo(peerInfo.id) < 0 then
        logger.info(s"Auto-connecting to discovered peer: ${peerInfo.displayName} at ${peerInfo.address}")
        connectToPeer(peerInfo).recover {
          case e: Exception =>
            logger.warn(s"Failed to auto-connect to ${peerInfo.displayName}: ${e.getMessage}")
            e.printStackTrace()
        }
      else
        logger.debug(s"Skipping auto-connect to ${peerInfo.displayName} (tie-breaking: waiting for them to connect to us)")
    else
      logger.debug(s"Skipping auto-connect - already connected to ${peerInfo.displayName}")

  /**
   * Handles a message received from a peer.
   */
  private def handleMessage(connection: PeerConnection, message: NetworkMessage): Unit =
    message match
      case NetworkMessage.Hello(peerId, hostname) =>
        logger.info(s"Received Hello from $hostname ($peerId)")

        // Check if this is a pending connection that needs its peer ID updated
        connection.remoteAddress.foreach { remoteAddr =>
          if pendingConnections.containsKey(remoteAddr) then
            // This is a pending connection - update the peer ID and move to main map
            logger.debug(s"Updating pending connection with real peer ID: $peerId")

            // Check if we're already connected to this peer
            if connections.containsKey(peerId) then
              logger.warn(s"Already connected to peer $peerId, closing duplicate connection")
              pendingConnections.remove(remoteAddr)
              connection.disconnect()
            else
              // Update the connection's peer info with the real peer ID
              val updatedPeerInfo = PeerInfo(peerId, remoteAddr, hostname)
              connection.updatePeerInfo(updatedPeerInfo)

              // Move from pending to active connections
              pendingConnections.remove(remoteAddr)
              connections.put(peerId, connection)
              logger.info(s"Activated connection to peer: ${updatedPeerInfo.displayName}")
              logger.debug(s"Total active connections: ${connections.size()}, peers: ${connections.keySet().asScala.mkString(", ")}")
        }

        // Forward to application
        onMessage(connection.peerInfo, message)

      case _ =>
        logger.debug(s"Received message from ${connection.peerInfo.displayName}: ${message.getClass.getSimpleName}")
        onMessage(connection.peerInfo, message)

  /**
   * Handles disconnection of a peer.
   */
  private def handleDisconnect(connection: PeerConnection): Unit =
    connections.remove(connection.peerInfo.id)
    logger.info(s"Peer disconnected: ${connection.peerInfo.displayName}")

object NetworkManager:

  /**
   * Default port for peer-to-peer connections.
   */
  val DEFAULT_PORT = 8888

  /**
   * Creates a network manager with default settings.
   */
  def apply(scheduler: Scheduler): NetworkManager =
    new NetworkManager(DEFAULT_PORT, scheduler)

  /**
   * Creates a network manager with a message handler.
   */
  def apply(scheduler: Scheduler, onMessage: (PeerInfo, NetworkMessage) => Unit): NetworkManager =
    new NetworkManager(DEFAULT_PORT, scheduler, onMessage)
