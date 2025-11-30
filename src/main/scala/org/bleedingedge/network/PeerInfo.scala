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
