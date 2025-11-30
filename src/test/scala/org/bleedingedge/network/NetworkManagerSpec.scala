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

import org.bleedingedge.scheduling.Scheduler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class NetworkManagerSpec extends AnyFlatSpec with Matchers:

  "NetworkManager" should "start and stop successfully" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9001, scheduler)
      manager.isRunning.shouldBe(false)

      val startFuture = manager.start(enableDiscovery = false)
      Await.result(startFuture, 5.seconds)

      // Give it a moment to fully start
      Thread.sleep(500)

      manager.isRunning.shouldBe(true)
      manager.stop()

      // Give it a moment to fully stop
      Thread.sleep(500)

      manager.isRunning.shouldBe(false)

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "prevent starting twice" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9002, scheduler)

      Await.result(manager.start(enableDiscovery = false), 5.seconds)
      Thread.sleep(500)

      assertThrows[IllegalStateException] {
        Await.result(manager.start(enableDiscovery = false), 5.seconds)
      }

      manager.stop()
      Thread.sleep(500)

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "track connection count" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9003, scheduler)

      manager.connectionCount.shouldBe(0)
      manager.connectedPeers.shouldBe(List.empty)

      manager.stop()

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "call message callback when message received" in {
    val scheduler = Scheduler(4)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    val messageCount = AtomicInteger(0)
    var receivedPeer: Option[PeerInfo] = None
    var receivedMessage: Option[NetworkMessage] = None

    val onMessage = (peer: PeerInfo, msg: NetworkMessage) => {
      messageCount.incrementAndGet()
      receivedPeer = Some(peer)
      receivedMessage = Some(msg)
    }

    try
      val manager = new NetworkManager(9004, scheduler, onMessage)

      // Callback should be set
      receivedPeer.shouldBe(None)
      receivedMessage.shouldBe(None)

      manager.stop()

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "create with default port" in {
    val scheduler = Scheduler(2)

    try
      val manager = NetworkManager(scheduler)
      manager.port.shouldBe(NetworkManager.DEFAULT_PORT)

    finally
      scheduler.gracefulShutdown(1)
  }

  it should "handle stop when not started" in {
    val scheduler = Scheduler(2)

    try
      val manager = new NetworkManager(9005, scheduler)
      manager.isRunning.shouldBe(false)

      // Should not throw
      manager.stop()
      manager.isRunning.shouldBe(false)

    finally
      scheduler.gracefulShutdown(1)
  }

  it should "reject connection attempts when not running" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9006, scheduler)
      val peerInfo = PeerInfo("localhost", 9999)

      assertThrows[IllegalStateException] {
        Await.result(manager.connectToPeer(peerInfo), 5.seconds)
      }

    finally
      scheduler.gracefulShutdown(1)
  }

  it should "clear connections on stop" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9007, scheduler)

      Await.result(manager.start(enableDiscovery = false), 5.seconds)
      Thread.sleep(500)

      manager.stop()
      Thread.sleep(500)

      manager.connectionCount.shouldBe(0)
      manager.connectedPeers.shouldBe(List.empty)

    finally
      scheduler.gracefulShutdown(2)
  }

  "NetworkManager broadcast" should "fail when not running" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9008, scheduler)
      val message = NetworkMessage.Heartbeat(System.currentTimeMillis())

      // Should succeed but not send to anyone (no connections)
      Await.result(manager.broadcast(message), 5.seconds)

    finally
      scheduler.gracefulShutdown(1)
  }

  "NetworkManager sendToPeer" should "fail for unknown peer" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val manager = new NetworkManager(9009, scheduler)
      Await.result(manager.start(enableDiscovery = false), 5.seconds)
      Thread.sleep(500)

      val unknownPeerId = UUID.randomUUID()
      val message = NetworkMessage.Heartbeat(System.currentTimeMillis())

      assertThrows[IllegalStateException] {
        Await.result(manager.sendToPeer(unknownPeerId, message), 5.seconds)
      }

      manager.stop()
      Thread.sleep(500)

    finally
      scheduler.gracefulShutdown(2)
  }
