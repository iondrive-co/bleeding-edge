/*
 * Copyright (c) 2025 Miles Hampson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.bleedingedge.network

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.scheduling.Scheduler

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
   * Sends a message to the peer.
   *
   * @param message The message to send
   * @return Future completing when message is sent
   */
  def sendMessage(message: NetworkMessage): Future[Unit] =
    outputStream.get() match
      case Some(out) if isConnected =>
        scheduler.execute("peer-send") {
          try
            // Synchronize to prevent concurrent writes
            sendLock.synchronized {
              message match
                case NetworkMessage.Hello(peerId, hostname) =>
                  out.writeByte(0)
                  out.writeLong(peerId.getMostSignificantBits)
                  out.writeLong(peerId.getLeastSignificantBits)
                  out.writeUTF(hostname)

                case NetworkMessage.Goodbye(peerId) =>
                  out.writeByte(1)
                  out.writeLong(peerId.getMostSignificantBits)
                  out.writeLong(peerId.getLeastSignificantBits)

                case NetworkMessage.StateBroadcast(states) =>
                  out.writeByte(2)
                  out.writeInt(states.length)
                  out.write(states)

                case NetworkMessage.StateRequest(hostname, resourceHash, resourceLength) =>
                  out.writeByte(3)
                  out.writeUTF(hostname)
                  out.writeUTF(resourceHash)
                  out.writeInt(resourceLength)

                case NetworkMessage.Heartbeat(timestamp) =>
                  out.writeByte(4)
                  out.writeLong(timestamp)

                case NetworkMessage.Acknowledgment(messageId) =>
                  out.writeByte(5)
                  out.writeLong(messageId.getMostSignificantBits)
                  out.writeLong(messageId.getLeastSignificantBits)

              out.flush()
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
   * Starts the receive loop for incoming messages.
   */
  private def startReceiveLoop(): Unit =
    scheduler.execute("peer-receive") {
      socket.get() match
        case Some(sock) =>
          try
            Using.resource(DataInputStream(sock.getInputStream)) { in =>
              while running.get() && isConnected do
                try
                  val messageType = in.readByte()
                  val message = messageType match
                    case 0 => // Hello
                      val mostSig = in.readLong()
                      val leastSig = in.readLong()
                      val peerId = UUID(mostSig, leastSig)
                      val hostname = in.readUTF()
                      NetworkMessage.Hello(peerId, hostname)

                    case 1 => // Goodbye
                      val mostSig = in.readLong()
                      val leastSig = in.readLong()
                      val peerId = UUID(mostSig, leastSig)
                      NetworkMessage.Goodbye(peerId)

                    case 2 => // StateBroadcast
                      val length = in.readInt()
                      val states = Array.ofDim[Byte](length)
                      in.readFully(states)
                      NetworkMessage.StateBroadcast(states)

                    case 3 => // StateRequest
                      val hostname = in.readUTF()
                      val resourceHash = in.readUTF()
                      val resourceLength = in.readInt()
                      NetworkMessage.StateRequest(hostname, resourceHash, resourceLength)

                    case 4 => // Heartbeat
                      val timestamp = in.readLong()
                      NetworkMessage.Heartbeat(timestamp)

                    case 5 => // Acknowledgment
                      val mostSig = in.readLong()
                      val leastSig = in.readLong()
                      val messageId = UUID(mostSig, leastSig)
                      NetworkMessage.Acknowledgment(messageId)

                    case unknown =>
                      throw IOException(s"Unknown message type: $unknown")

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
