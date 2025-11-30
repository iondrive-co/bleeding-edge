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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import java.util.UUID

class PeerInfoSpec extends AnyFlatSpec with Matchers:

  "PeerInfo" should "create from address and hostname" in {
    val address = InetSocketAddress("192.168.1.100", 8888)
    val peerInfo = PeerInfo(UUID.randomUUID(), address, "test-host")

    peerInfo.address.shouldBe(address)
    peerInfo.hostname.shouldBe("test-host")
  }

  it should "create from host and port" in {
    val peerInfo = PeerInfo("localhost", 9999)

    peerInfo.address.getHostString.shouldBe("localhost")
    peerInfo.address.getPort.shouldBe(9999)
    peerInfo.hostname.shouldBe("localhost")
  }

  it should "generate display name with hostname" in {
    val id = UUID.randomUUID()
    val peerInfo = PeerInfo(id, InetSocketAddress("192.168.1.1", 8888), "my-computer")

    peerInfo.displayName should include("my-computer")
    peerInfo.displayName should include(id.toString)
  }

  it should "generate display name without hostname" in {
    val id = UUID.randomUUID()
    val peerInfo = PeerInfo(id, InetSocketAddress("192.168.1.1", 8888), "")

    peerInfo.displayName should include("192.168.1.1")
    peerInfo.displayName should include("8888")
    peerInfo.displayName should include(id.toString)
  }

  it should "match addresses correctly" in {
    val address1 = InetSocketAddress("192.168.1.1", 8888)
    val address2 = InetSocketAddress("192.168.1.1", 8888)
    val address3 = InetSocketAddress("192.168.1.2", 8888)

    val peerInfo = PeerInfo(UUID.randomUUID(), address1, "test")

    peerInfo.matchesAddress(address2).shouldBe(true)
    peerInfo.matchesAddress(address3).shouldBe(false)
  }

  "ConnectionState" should "have all expected states" in {
    ConnectionState.Disconnected.shouldBe(an[ConnectionState])
    ConnectionState.Connecting.shouldBe(an[ConnectionState])
    ConnectionState.Connected.shouldBe(an[ConnectionState])
    ConnectionState.Failed("test").shouldBe(an[ConnectionState])
  }

  it should "store failure reason" in {
    val failed = ConnectionState.Failed("connection timeout")
    failed match
      case ConnectionState.Failed(reason) => reason.shouldBe("connection timeout")
      case _ => fail("Expected Failed state")
  }

  "NetworkMessage" should "support Hello messages" in {
    val peerId = UUID.randomUUID()
    val msg = NetworkMessage.Hello(peerId, "hostname")

    msg match
      case NetworkMessage.Hello(id, host) =>
        id.shouldBe(peerId)
        host.shouldBe("hostname")
      case _ => fail("Expected Hello message")
  }

  it should "support Goodbye messages" in {
    val peerId = UUID.randomUUID()
    val msg = NetworkMessage.Goodbye(peerId)

    msg match
      case NetworkMessage.Goodbye(id) => id.shouldBe(peerId)
      case _ => fail("Expected Goodbye message")
  }

  it should "support StateBroadcast messages" in {
    val data = "test data".getBytes
    val msg = NetworkMessage.StateBroadcast(data)

    msg match
      case NetworkMessage.StateBroadcast(states) =>
        states.sameElements(data).shouldBe(true)
      case _ => fail("Expected StateBroadcast message")
  }

  it should "support StateRequest messages" in {
    val msg = NetworkMessage.StateRequest("host", "hash123", 1024)

    msg match
      case NetworkMessage.StateRequest(host, hash, length) =>
        host.shouldBe("host")
        hash.shouldBe("hash123")
        length.shouldBe(1024)
      case _ => fail("Expected StateRequest message")
  }

  it should "support Heartbeat messages" in {
    val timestamp = System.currentTimeMillis()
    val msg = NetworkMessage.Heartbeat(timestamp)

    msg match
      case NetworkMessage.Heartbeat(ts) => ts.shouldBe(timestamp)
      case _ => fail("Expected Heartbeat message")
  }

  it should "support Acknowledgment messages" in {
    val messageId = UUID.randomUUID()
    val msg = NetworkMessage.Acknowledgment(messageId)

    msg match
      case NetworkMessage.Acknowledgment(id) => id.shouldBe(messageId)
      case _ => fail("Expected Acknowledgment message")
  }
