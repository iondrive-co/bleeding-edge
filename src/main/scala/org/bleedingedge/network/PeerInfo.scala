package org.bleedingedge.network

import java.net.InetSocketAddress
import java.util.UUID

/**
 * Immutable representation of a peer in the network.
 *
 * @param id Unique identifier for this peer
 * @param address Network address of the peer
 * @param hostname Optional hostname for display purposes
 * @since 2.0.0
 */
case class PeerInfo(
  id: UUID,
  address: InetSocketAddress,
  hostname: String = ""
):

  /**
   * Returns a human-readable representation of this peer.
   */
  def displayName: String =
    if hostname.nonEmpty then s"$hostname ($id)"
    else s"${address.getHostString}:${address.getPort} ($id)"

  /**
   * Returns true if this peer matches the given address.
   */
  def matchesAddress(addr: InetSocketAddress): Boolean =
    address.getHostString == addr.getHostString && address.getPort == addr.getPort

object PeerInfo:

  /**
   * Creates a PeerInfo with a generated UUID.
   */
  def apply(address: InetSocketAddress, hostname: String): PeerInfo =
    PeerInfo(UUID.randomUUID(), address, hostname)

  /**
   * Creates a PeerInfo from host and port.
   */
  def apply(host: String, port: Int): PeerInfo =
    PeerInfo(UUID.randomUUID(), InetSocketAddress(host, port), host)

/**
 * Connection state of a peer.
 */
enum ConnectionState:
  case Disconnected
  case Connecting
  case Connected
  case Failed(reason: String)

/**
 * Types of network messages exchanged between peers.
 */
enum NetworkMessage:
  case Hello(peerId: UUID, hostname: String)
  case Goodbye(peerId: UUID)
  case StateBroadcast(states: Array[Byte])
  case StateRequest(hostname: String, resourceHash: String, resourceLength: Int)
  case Heartbeat(timestamp: Long)
  case Acknowledgment(messageId: UUID)
