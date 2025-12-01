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

package org.bleedingedge.integration

import org.bleedingedge.infrastructure.scheduling.Scheduler
import org.bleedingedge.network.NetworkManager
import org.bleedingedge.sync.SyncManager
import org.scalatest.Ignore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

/**
 * Integration tests for end-to-end peer-to-peer file synchronization.
 *
 * Tests the complete stack: FileSystemMonitor -> SyncManager -> NetworkManager -> PeerConnection
 *
 * NOTE: These tests are currently ignored as they require full end-to-end implementation.
 * They serve as a specification for the complete synchronization flow and will be
 * enabled once the following are implemented:
 * - Reliable file change detection in FileSystemMonitor
 * - State accumulation and snapshot creation timing
 * - Message delivery guarantees
 * - Command execution error handling
 */
@Ignore
class SyncIntegrationSpec extends AnyFlatSpec with Matchers:

  /**
   * Helper class to manage a peer (SyncManager + NetworkManager) for testing.
   */
  case class TestPeer(
    name: String,
    syncDir: Path,
    syncManager: SyncManager,
    networkManager: NetworkManager,
    scheduler: Scheduler
  ):
    def cleanup(): Unit =
      Try {
        syncManager.stop()
        networkManager.stop()
        scheduler.gracefulShutdown(2)
        deleteDirectory(syncDir)
      }

    private def deleteDirectory(path: Path): Unit =
      if Files.exists(path) then
        Files.walk(path)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)

  /**
   * Creates a test peer with its own directory and network configuration.
   */
  def createTestPeer(name: String, port: Int): TestPeer =
    val scheduler = Scheduler(4)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    // Create temporary sync directory
    val syncDir = Files.createTempDirectory(s"sync-$name-")

    // Create sync manager
    val syncManager = SyncManager(syncDir, scheduler)

    // Create network manager with sync manager's message handler
    val networkManager = new NetworkManager(port, scheduler, syncManager.handleMessage)

    // Connect sync manager to network manager
    syncManager.connect(networkManager)

    TestPeer(name, syncDir, syncManager, networkManager, scheduler)

  /**
   * Connects two peers together.
   */
  def connectPeers(peer1: TestPeer, peer2: TestPeer): Unit =
    given scala.concurrent.ExecutionContext = peer1.scheduler.executionContext

    // Start both network managers
    Await.result(peer1.networkManager.start(enableDiscovery = false), 5.seconds)
    Await.result(peer2.networkManager.start(enableDiscovery = false), 5.seconds)

    // Start both sync managers
    Await.result(peer1.syncManager.start(), 5.seconds)
    Await.result(peer2.syncManager.start(), 5.seconds)

    // Give them a moment to fully start
    Thread.sleep(500)

    // Connect peer1 to peer2
    val peer2Info = org.bleedingedge.network.PeerInfo("localhost", peer2.networkManager.port)
    Await.result(peer1.networkManager.connectToPeer(peer2Info), 5.seconds)

    // Give the connection time to establish
    Thread.sleep(500)

  /**
   * Writes a file to a peer's sync directory.
   */
  def writeFile(peer: TestPeer, relativePath: String, content: String): Path =
    val filePath = peer.syncDir.resolve(relativePath)
    Files.createDirectories(filePath.getParent)
    Files.writeString(filePath, content)
    filePath

  /**
   * Reads a file from a peer's sync directory.
   */
  def readFile(peer: TestPeer, relativePath: String): Option[String] =
    val filePath = peer.syncDir.resolve(relativePath)
    if Files.exists(filePath) then
      Some(Files.readString(filePath))
    else
      None

  /**
   * Waits for a file to exist at a peer with expected content.
   */
  def waitForFile(peer: TestPeer, relativePath: String, expectedContent: String, timeout: Duration = 10.seconds): Boolean =
    val startTime = System.currentTimeMillis()
    val timeoutMillis = timeout.toMillis
    while System.currentTimeMillis() - startTime < timeoutMillis do
      readFile(peer, relativePath) match
        case Some(content) if content == expectedContent =>
          return true
        case _ =>
          Thread.sleep(100)
    false

  "Two-peer synchronization" should "sync file creation from peer1 to peer2" in {
    val peer1 = createTestPeer("peer1", 9200)
    val peer2 = createTestPeer("peer2", 9201)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Create a file on peer1
      writeFile(peer1, "test.txt", "Hello from peer1")

      // Wait for file to sync to peer2
      waitForFile(peer2, "test.txt", "Hello from peer1").shouldBe(true)

      // Verify content matches
      readFile(peer2, "test.txt").shouldBe(Some("Hello from peer1"))

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "sync file creation from peer2 to peer1" in {
    val peer1 = createTestPeer("peer1", 9202)
    val peer2 = createTestPeer("peer2", 9203)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Create a file on peer2
      writeFile(peer2, "test.txt", "Hello from peer2")

      // Wait for file to sync to peer1
      waitForFile(peer1, "test.txt", "Hello from peer2").shouldBe(true)

      // Verify content matches
      readFile(peer1, "test.txt").shouldBe(Some("Hello from peer2"))

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "sync file updates between peers" in {
    val peer1 = createTestPeer("peer1", 9204)
    val peer2 = createTestPeer("peer2", 9205)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Create initial file on peer1
      writeFile(peer1, "test.txt", "Version 1")
      waitForFile(peer2, "test.txt", "Version 1").shouldBe(true)

      // Update file on peer1
      writeFile(peer1, "test.txt", "Version 2")
      waitForFile(peer2, "test.txt", "Version 2").shouldBe(true)

      // Verify final content
      readFile(peer2, "test.txt").shouldBe(Some("Version 2"))

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "sync file deletion between peers" in {
    val peer1 = createTestPeer("peer1", 9206)
    val peer2 = createTestPeer("peer2", 9207)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Create file on peer1
      writeFile(peer1, "test.txt", "To be deleted")
      waitForFile(peer2, "test.txt", "To be deleted").shouldBe(true)

      // Delete file on peer1
      Files.delete(peer1.syncDir.resolve("test.txt"))

      // Wait for deletion to sync (file should not exist)
      Thread.sleep(2000) // Give it time to sync the deletion

      // Verify file is deleted on peer2
      Files.exists(peer2.syncDir.resolve("test.txt")).shouldBe(false)

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "sync multiple files between peers" in {
    val peer1 = createTestPeer("peer1", 9208)
    val peer2 = createTestPeer("peer2", 9209)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Create multiple files on peer1
      writeFile(peer1, "file1.txt", "Content 1")
      writeFile(peer1, "file2.txt", "Content 2")
      writeFile(peer1, "file3.txt", "Content 3")

      // Wait for all files to sync
      waitForFile(peer2, "file1.txt", "Content 1").shouldBe(true)
      waitForFile(peer2, "file2.txt", "Content 2").shouldBe(true)
      waitForFile(peer2, "file3.txt", "Content 3").shouldBe(true)

      // Verify all content matches
      readFile(peer2, "file1.txt").shouldBe(Some("Content 1"))
      readFile(peer2, "file2.txt").shouldBe(Some("Content 2"))
      readFile(peer2, "file3.txt").shouldBe(Some("Content 3"))

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "handle peers starting with existing files" in {
    val peer1 = createTestPeer("peer1", 9210)
    val peer2 = createTestPeer("peer2", 9211)

    try
      // Create files on peer1 BEFORE starting sync
      writeFile(peer1, "existing.txt", "Existing content")

      // Now connect peers (this should sync existing files)
      connectPeers(peer1, peer2)

      // Wait for existing file to sync
      waitForFile(peer2, "existing.txt", "Existing content").shouldBe(true)

      // Verify content matches
      readFile(peer2, "existing.txt").shouldBe(Some("Existing content"))

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  "Connection management" should "maintain connection between peers" in {
    val peer1 = createTestPeer("peer1", 9212)
    val peer2 = createTestPeer("peer2", 9213)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      // Verify connection exists
      peer1.networkManager.connectionCount.shouldBe(1)

      // Create and sync a file to verify connection works
      writeFile(peer1, "test.txt", "Connection test")
      waitForFile(peer2, "test.txt", "Connection test").shouldBe(true)

    finally
      peer1.cleanup()
      peer2.cleanup()
  }

  it should "handle peer disconnection gracefully" in {
    val peer1 = createTestPeer("peer1", 9214)
    val peer2 = createTestPeer("peer2", 9215)

    try
      // Connect peers
      connectPeers(peer1, peer2)

      peer1.networkManager.connectionCount.shouldBe(1)

      // Stop peer2
      peer2.syncManager.stop()
      peer2.networkManager.stop()

      // Give it time to detect disconnection
      Thread.sleep(1000)

      // Peer1 should detect disconnection
      peer1.networkManager.connectionCount.shouldBe(0)

    finally
      peer1.cleanup()
      peer2.cleanup()
  }
