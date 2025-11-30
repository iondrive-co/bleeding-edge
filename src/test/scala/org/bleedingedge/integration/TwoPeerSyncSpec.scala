/*
 * Integration test for two-peer file synchronization
 */

package org.bleedingedge.integration

import org.bleedingedge.network.NetworkManager
import org.bleedingedge.scheduling.Scheduler
import org.bleedingedge.sync.SyncManager
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

class TwoPeerSyncSpec extends AnyFunSuite with Matchers with BeforeAndAfterEach:

  var testDir1: Path = scala.compiletime.uninitialized
  var testDir2: Path = scala.compiletime.uninitialized
  var scheduler1: Scheduler = scala.compiletime.uninitialized
  var scheduler2: Scheduler = scala.compiletime.uninitialized
  var syncManager1: SyncManager = scala.compiletime.uninitialized
  var syncManager2: SyncManager = scala.compiletime.uninitialized
  var networkManager1: NetworkManager = scala.compiletime.uninitialized
  var networkManager2: NetworkManager = scala.compiletime.uninitialized

  override def beforeEach(): Unit =
    // Create test directories
    testDir1 = Files.createTempDirectory("peer1-")
    testDir2 = Files.createTempDirectory("peer2-")

    // Create schedulers with enough threads for:
    // - network-accept (blocking)
    // - peer-discovery-listen (blocking)
    // - peer-discovery-broadcast
    // - file-monitor
    // - peer-connect, peer-send, peer-receive
    scheduler1 = Scheduler(10)
    scheduler2 = Scheduler(10)

  override def afterEach(): Unit =
    // Clean up
    Try {
      if syncManager1 != null then syncManager1.stop()
      if syncManager2 != null then syncManager2.stop()
      if networkManager1 != null then networkManager1.stop()
      if networkManager2 != null then networkManager2.stop()
      if scheduler1 != null then scheduler1.gracefulShutdown(1)
      if scheduler2 != null then scheduler2.gracefulShutdown(1)

      // Delete test directories
      def deleteRecursively(path: Path): Unit =
        if Files.exists(path) then
          if Files.isDirectory(path) then
            Files.list(path).forEach(deleteRecursively)
          Files.delete(path)

      deleteRecursively(testDir1)
      deleteRecursively(testDir2)
    }

  test("Two peers should sync files between different directories"):
    // Create a file in peer1's directory
    val file1 = testDir1.resolve("file1.txt")
    Files.writeString(file1, "Content from peer 1")

    // Create a different file in peer2's directory
    val file2 = testDir2.resolve("file2.txt")
    Files.writeString(file2, "Content from peer 2")

    // Set up peer 1
    syncManager1 = SyncManager(testDir1, scheduler1)
    networkManager1 = new NetworkManager(0, scheduler1, syncManager1.handleMessage)
    syncManager1.connect(networkManager1)

    given scala.concurrent.ExecutionContext = scheduler1.executionContext

    Await.result(networkManager1.start(enableDiscovery = true), 5.seconds)
    Await.result(syncManager1.start(), 5.seconds)

    val port1 = networkManager1.getActualPort

    // Set up peer 2
    syncManager2 = SyncManager(testDir2, scheduler2)
    networkManager2 = new NetworkManager(0, scheduler2, syncManager2.handleMessage)
    syncManager2.connect(networkManager2)

    Await.result(networkManager2.start(enableDiscovery = true), 5.seconds)
    Await.result(syncManager2.start(), 5.seconds)

    val port2 = networkManager2.getActualPort

    println(s"Peer 1 on port $port1, Peer 2 on port $port2")
    println(s"Peer 1 initial snapshot: ${syncManager1.localSnapshot.states.size} files")
    println(s"Peer 2 initial snapshot: ${syncManager2.localSnapshot.states.size} files")

    // Wait for discovery and sync with retry (multicast can be slow, especially under load)
    println("Waiting for discovery and sync...")
    val file2InPeer1 = testDir1.resolve("file2.txt")
    val file1InPeer2 = testDir2.resolve("file1.txt")

    // Poll for files to exist with timeout (max 20 seconds)
    val maxRetries = 40
    var retries = 0
    var bothFilesSynced = false
    while retries < maxRetries && !bothFilesSynced do
      Thread.sleep(500)
      retries += 1
      if Files.exists(file2InPeer1) && Files.exists(file1InPeer2) then
        bothFilesSynced = true
        println(s"Files synced after ${retries * 500}ms")

    // Check that peer1 now has file2.txt
    println(s"Peer 1 has file2.txt? ${Files.exists(file2InPeer1)}")
    if Files.exists(file2InPeer1) then
      println(s"  Content: ${Files.readString(file2InPeer1)}")

    // Check that peer2 now has file1.txt
    println(s"Peer 2 has file1.txt? ${Files.exists(file1InPeer2)}")
    if Files.exists(file1InPeer2) then
      println(s"  Content: ${Files.readString(file1InPeer2)}")

    // Assertions
    Files.exists(file2InPeer1) shouldBe true
    Files.readString(file2InPeer1) shouldBe "Content from peer 2"

    Files.exists(file1InPeer2) shouldBe true
    Files.readString(file1InPeer2) shouldBe "Content from peer 1"
