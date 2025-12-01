package org.bleedingedge.network

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.codec.Serialization
import org.bleedingedge.infrastructure.scheduling.Scheduler

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetSocketAddress, Socket}
import java.util.UUID
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try, Using}

/**
 * Manages a TCP connection to a single peer.
 *
 * Provides thread-safe, non-blocking operations for:
 * - Establishing connections
 * - Sending messages
 * - Receiving messages via callback
 * - Connection lifecycle management
 *
 * @param localPeerId The local peer's ID
 * @param peerInfo Information about the remote peer
 * @param scheduler Scheduler for async operations
 * @param onMessage Callback invoked when a message is received
 * @param onDisconnect Callback invoked when connection is lost
 * @since 2.0.0
 */
class PeerConnection(
  val localPeerId: UUID,
  initialPeerInfo: PeerInfo,
  scheduler: Scheduler,
  onMessage: (PeerConnection, NetworkMessage) => Unit,
  onDisconnect: PeerConnection => Unit
) extends LazyLogging:

  private val socket: AtomicReference[Option[Socket]] = AtomicReference(None)
  private val state: AtomicReference[ConnectionState] = AtomicReference(ConnectionState.Disconnected)
  private val running: AtomicBoolean = AtomicBoolean(false)
  private val _peerInfo: AtomicReference[PeerInfo] = AtomicReference(initialPeerInfo)
  private val outputStream: AtomicReference[Option[DataOutputStream]] = AtomicReference(None)
  private val sendLock = new Object()  // Synchronize writes to prevent concurrent access

  given scala.concurrent.ExecutionContext = scheduler.executionContext

  /**
   * Returns the current peer info (may be updated when Hello is received).
   */
  def peerInfo: PeerInfo = _peerInfo.get()

  /**
   * Updates the peer info (called when Hello message reveals the real peer ID).
   */
  def updatePeerInfo(newPeerInfo: PeerInfo): Unit =
    _peerInfo.set(newPeerInfo)
    logger.info(s"Updated peer info to: ${newPeerInfo.displayName}")

  /**
   * Returns the remote address of this connection, if connected.
   */
  def remoteAddress: Option[InetSocketAddress] =
    socket.get().map(_.getRemoteSocketAddress.asInstanceOf[InetSocketAddress])

  /**
   * Returns the current connection state.
   */
  def connectionState: ConnectionState = state.get()

  /**
   * Returns true if this connection is currently active.
   */
  def isConnected: Boolean = connectionState == ConnectionState.Connected

  /**
   * Establishes a connection to the peer.
   *
   * @param timeout Connection timeout in milliseconds
   * @return Future completing when connection is established
   */
  def connect(timeout: Int = 5000): Future[Unit] =
    if state.compareAndSet(ConnectionState.Disconnected, ConnectionState.Connecting) then
      logger.info(s"Connecting to peer: ${peerInfo.displayName}")

      scheduler.execute("peer-connect") {
        try
          val sock = Socket()
          sock.connect(peerInfo.address, timeout)
          sock.setKeepAlive(true)
          sock.setTcpNoDelay(true)

          // Create output stream once for the lifetime of this connection
          val out = DataOutputStream(sock.getOutputStream)

          socket.set(Some(sock))
          outputStream.set(Some(out))
          state.set(ConnectionState.Connected)
          running.set(true)

          logger.info(s"Connected to peer: ${peerInfo.displayName}")

          // Send hello message with our real peer ID
          sendMessage(NetworkMessage.Hello(localPeerId, java.net.InetAddress.getLocalHost.getHostName))

          // Start receive loop
          startReceiveLoop()

        catch
          case e: IOException =>
            logger.error(s"Failed to connect to ${peerInfo.displayName}", e)
            state.set(ConnectionState.Failed(e.getMessage))
            socket.set(None)
            outputStream.set(None)
            throw e
      }
    else
      Future.failed(IllegalStateException(s"Cannot connect in state: ${state.get()}"))

  /**
   * Sends a message to the peer using the Codec layer for serialization.
   *
   * @param message The message to send
   * @return Future completing when message is sent
   */
  def sendMessage(message: NetworkMessage): Future[Unit] =
    outputStream.get() match
      case Some(out) if isConnected =>
        scheduler.execute("peer-send") {
          try
            // Synchronize to prevent concurrent writes and use Codec layer
            sendLock.synchronized {
              Serialization.writeNetworkMessage(out, message)
            }
            logger.debug(s"Sent message to ${peerInfo.displayName}: ${message.getClass.getSimpleName}")
          catch
            case e: IOException =>
              logger.error(s"Error sending message to ${peerInfo.displayName}", e)
              disconnect()
              throw e
        }

      case _ =>
        Future.failed(IllegalStateException("Not connected"))

  /**
   * Starts the receive loop for incoming messages using the Codec layer.
   */
  private def startReceiveLoop(): Unit =
    scheduler.execute("peer-receive") {
      socket.get() match
        case Some(sock) =>
          try
            Using.resource(DataInputStream(sock.getInputStream)) { in =>
              while running.get() && isConnected do
                try
                  // Use Codec layer for deserialization
                  val message = Serialization.readNetworkMessage(in)

                  logger.debug(s"Received message from ${peerInfo.displayName}: ${message.getClass.getSimpleName}")
                  onMessage(this, message)

                catch
                  case _: java.io.EOFException =>
                    logger.info(s"Peer disconnected: ${peerInfo.displayName}")
                    running.set(false)
                  case e: IOException =>
                    logger.error(s"Error receiving message from ${peerInfo.displayName}", e)
                    running.set(false)
            }
          catch
            case e: Exception =>
              logger.error(s"Receive loop error for ${peerInfo.displayName}", e)
          finally
            disconnect()

        case None =>
          logger.warn("Receive loop started but no socket available")
    }

  /**
   * Disconnects from the peer.
   */
  def disconnect(): Unit =
    if running.compareAndSet(true, false) || state.get() == ConnectionState.Connected then
      logger.info(s"Disconnecting from peer: ${peerInfo.displayName}")

      // Send goodbye if connected
      if isConnected then
        Try {
          sendMessage(NetworkMessage.Goodbye(localPeerId))
        }

      // Close output stream first
      outputStream.getAndSet(None).foreach { out =>
        Try(out.close())
      }

      // Close socket
      socket.getAndSet(None).foreach { sock =>
        Try(sock.close())
      }

      state.set(ConnectionState.Disconnected)
      onDisconnect(this)

object PeerConnection:

  /**
   * Creates a new peer connection from an accepted socket.
   *
   * @param localPeerId The local peer's ID
   * @param socket The accepted socket
   * @param scheduler Scheduler for async operations
   * @param onMessage Message callback
   * @param onDisconnect Disconnect callback
   * @return New PeerConnection instance
   */
  def fromSocket(
    localPeerId: UUID,
    socket: Socket,
    scheduler: Scheduler,
    onMessage: (PeerConnection, NetworkMessage) => Unit,
    onDisconnect: PeerConnection => Unit
  ): PeerConnection =
    val remoteAddress = socket.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]
    // Use a temporary unknown peer ID - will be updated when Hello is received
    val peerInfo = PeerInfo(UUID.randomUUID(), remoteAddress, remoteAddress.getHostString)

    val connection = PeerConnection(localPeerId, peerInfo, scheduler, onMessage, onDisconnect)

    // Create output stream once for this connection
    val out = DataOutputStream(socket.getOutputStream)

    connection.socket.set(Some(socket))
    connection.outputStream.set(Some(out))
    connection.state.set(ConnectionState.Connected)
    connection.running.set(true)

    // Send Hello message to the remote peer
    connection.sendMessage(NetworkMessage.Hello(localPeerId, java.net.InetAddress.getLocalHost.getHostName))

    // Start receive loop immediately for accepted connections
    connection.startReceiveLoop()

    connection
