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

import java.net.*
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future
import scala.util.{Try, Using}

/**
 * Discovers peers on the local network using IP multicast.
 *
 * Periodically broadcasts presence announcements and listens for
 * announcements from other peers.
 *
 * @param localPeerId The peer ID to broadcast
 * @param localPort The local port to listen on
 * @param multicastGroup The multicast group address (e.g., "230.0.0.1")
 * @param multicastPort The multicast port
 * @param scheduler Scheduler for async operations
 * @param onPeerDiscovered Callback when a new peer is discovered
 * @since 2.0.0
 */
class PeerDiscovery(
  val localPeerId: UUID,
  val localPort: Int,
  val multicastGroup: String = "230.0.0.1",
  val multicastPort: Int = 4446,
  scheduler: Scheduler,
  onPeerDiscovered: PeerInfo => Unit
) extends LazyLogging:

  private val running = AtomicBoolean(false)
  private val localHostname = InetAddress.getLocalHost.getHostName

  given scala.concurrent.ExecutionContext = scheduler.executionContext

  /**
   * Starts peer discovery.
   *
   * Begins listening for peer announcements and broadcasting our presence.
   *
   * @return Future that completes immediately after starting background tasks
   */
  def start(): Future[Unit] =
    if running.compareAndSet(false, true) then
      logger.info(s"Starting peer discovery on $multicastGroup:$multicastPort (local peer: $localPeerId)")

      // Start listening and broadcasting in background (don't wait for them)
      startListening()
      startBroadcasting()

      // Return immediately - the tasks run in background
      Future.successful(())
    else
      Future.failed(IllegalStateException("Peer discovery already running"))

  /**
   * Stops peer discovery.
   */
  def stop(): Unit =
    if running.compareAndSet(true, false) then
      logger.info("Stopping peer discovery")

  /**
   * Returns true if discovery is currently running.
   */
  def isRunning: Boolean = running.get()

  /**
   * Starts listening for peer announcements.
   */
  private def startListening(): Future[Unit] =
    scheduler.execute("peer-discovery-listen") {
      Try {
        Using.resource(MulticastSocket(multicastPort)) { socket =>
          val group = InetAddress.getByName(multicastGroup)
          val networkInterface = NetworkInterface.getByInetAddress(InetAddress.getLocalHost)

          socket.joinGroup(InetSocketAddress(group, multicastPort), networkInterface)
          logger.info(s"Joined multicast group $multicastGroup:$multicastPort")

          val buffer = Array.ofDim[Byte](1024)
          val packet = DatagramPacket(buffer, buffer.length)

          while running.get() do
            try
              socket.receive(packet)
              val data = String(packet.getData, 0, packet.getLength, "UTF-8")

              // Parse announcement: "PEER|<uuid>|<hostname>|<port>"
              data.split("\\|") match
                case Array("PEER", peerIdStr, hostname, portStr) =>
                  val peerId = UUID.fromString(peerIdStr)
                  val port = portStr.toInt

                  // Ignore our own announcements
                  logger.debug(s"Received announcement: peerId=$peerId, localPeerId=$localPeerId, equals=${peerId == localPeerId}")
                  if peerId != localPeerId then
                    val peerAddress = InetSocketAddress(packet.getAddress, port)
                    val peerInfo = PeerInfo(peerId, peerAddress, hostname)

                    logger.info(s"Discovered peer: ${peerInfo.displayName}")
                    onPeerDiscovered(peerInfo)
                  else
                    logger.debug(s"Ignoring own announcement from $hostname")

                case _ =>
                  logger.warn(s"Received invalid announcement: $data")

            catch
              case _: java.net.SocketTimeoutException =>
                // Normal timeout, continue
              case e: Exception =>
                logger.error("Error receiving discovery packet", e)

          socket.leaveGroup(InetSocketAddress(group, multicastPort), networkInterface)
        }
      }.recover {
        case e: Exception =>
          logger.error("Error in discovery listener", e)
      }
    }

  /**
   * Starts broadcasting presence announcements.
   */
  private def startBroadcasting(): Future[Unit] =
    scheduler.execute("peer-discovery-broadcast") {
      Try {
        Using.resource(MulticastSocket()) { socket =>
          val group = InetAddress.getByName(multicastGroup)
          val announcement = s"PEER|$localPeerId|$localHostname|$localPort"
          val data = announcement.getBytes("UTF-8")

          logger.info(s"Broadcasting presence: $announcement")

          while running.get() do
            try
              val packet = DatagramPacket(data, data.length, group, multicastPort)
              socket.send(packet)
              logger.debug("Sent presence announcement")

              // Broadcast every 5 seconds
              Thread.sleep(5000)

            catch
              case _: InterruptedException =>
                // Stop broadcasting
                running.set(false)
              case e: Exception =>
                logger.error("Error sending discovery packet", e)
        }
      }.recover {
        case e: Exception =>
          logger.error("Error in discovery broadcaster", e)
      }
    }

object PeerDiscovery:

  /**
   * Default multicast group for peer discovery.
   */
  val DEFAULT_MULTICAST_GROUP = "230.0.0.1"

  /**
   * Default multicast port for peer discovery.
   */
  val DEFAULT_MULTICAST_PORT = 4446

  /**
   * Creates a peer discovery instance with default settings.
   */
  def apply(
    localPeerId: UUID,
    localPort: Int,
    scheduler: Scheduler,
    onPeerDiscovered: PeerInfo => Unit
  ): PeerDiscovery =
    new PeerDiscovery(
      localPeerId,
      localPort,
      DEFAULT_MULTICAST_GROUP,
      DEFAULT_MULTICAST_PORT,
      scheduler,
      onPeerDiscovered
    )
