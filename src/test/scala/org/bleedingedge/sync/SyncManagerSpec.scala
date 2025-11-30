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

package org.bleedingedge.sync

import org.bleedingedge.codec.Serialization
import org.bleedingedge.domain.{LocationState, Snapshot}
import org.bleedingedge.network.{NetworkManager, NetworkMessage, PeerInfo}
import org.bleedingedge.scheduling.Scheduler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration.*

class SyncManagerSpec extends AnyFlatSpec with Matchers:

  "SyncManager" should "start and stop successfully" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      syncManager.isRunning.shouldBe(false)

      Await.result(syncManager.start(), 5.seconds)
      syncManager.isRunning.shouldBe(true)

      syncManager.stop()
      syncManager.isRunning.shouldBe(false)

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "prevent starting twice" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)

      Await.result(syncManager.start(), 5.seconds)

      assertThrows[IllegalStateException] {
        Await.result(syncManager.start(), 5.seconds)
      }

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "initialize with empty snapshot" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      syncManager.localSnapshot.shouldBe(Snapshot.empty)

    finally
      scheduler.gracefulShutdown(1)
      Files.deleteIfExists(tempDir)
  }

  it should "connect to network manager" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      val networkManager = new NetworkManager(9100, scheduler)

      // Should not throw
      syncManager.connect(networkManager)

    finally
      scheduler.gracefulShutdown(1)
      Files.deleteIfExists(tempDir)
  }

  it should "handle Hello message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val peerId = UUID.randomUUID()
      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.Hello(peerId, "test-host")

      // Should not throw
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "handle StateBroadcast message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val peerInfo = PeerInfo("localhost", 9999)

      // Create some test states
      val state1 = LocationState("/test/file1.txt", "content1".getBytes)
      val state2 = LocationState("/test/file2.txt", "content2".getBytes)
      val stateBytes = Serialization.statesToBytes(List(state1, state2))

      val message = NetworkMessage.StateBroadcast(stateBytes)

      // Should not throw
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "handle StateRequest message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.StateRequest("test-host", "abc123", 100)

      // Should not throw (even though we don't have the requested state)
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "handle Heartbeat message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.Heartbeat(System.currentTimeMillis())

      // Should not throw
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "handle Goodbye message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val peerId = UUID.randomUUID()
      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.Goodbye(peerId)

      // Should not throw
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "handle Acknowledgment message" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      Await.result(syncManager.start(), 5.seconds)

      val messageId = UUID.randomUUID()
      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.Acknowledgment(messageId)

      // Should not throw
      syncManager.handleMessage(peerInfo, message)

      syncManager.stop()

    finally
      scheduler.gracefulShutdown(2)
      Files.deleteIfExists(tempDir)
  }

  it should "ignore messages when not running" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      syncManager.isRunning.shouldBe(false)

      val peerInfo = PeerInfo("localhost", 9999)
      val message = NetworkMessage.Heartbeat(System.currentTimeMillis())

      // Should not throw and should be ignored
      syncManager.handleMessage(peerInfo, message)

    finally
      scheduler.gracefulShutdown(1)
      Files.deleteIfExists(tempDir)
  }

  it should "handle stop when not started" in {
    val scheduler = Scheduler(2)
    val tempDir = Files.createTempDirectory("sync-test")

    try
      val syncManager = SyncManager(tempDir, scheduler)
      syncManager.isRunning.shouldBe(false)

      // Should not throw
      syncManager.stop()
      syncManager.isRunning.shouldBe(false)

    finally
      scheduler.gracefulShutdown(1)
      Files.deleteIfExists(tempDir)
  }
