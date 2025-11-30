/*
 * Test for merge-style P2P synchronization behavior
 */

package org.bleedingedge.transposition

import org.bleedingedge.domain.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MergeSyncSpec extends AnyFunSuite with Matchers:

  test("commandsBetween should generate only Create commands for merge sync"):
    // Peer 1 has file1.txt
    val peer1State = LocationState("file1.txt", "Content 1".getBytes)
    val peer1Snapshot = Snapshot(peer1State)

    // Peer 2 has file2.txt
    val peer2State = LocationState("file2.txt", "Content 2".getBytes)
    val peer2Snapshot = Snapshot(peer2State)

    // When peer 1 receives peer 2's snapshot, it should only CREATE file2.txt
    // It should NOT delete file1.txt!
    val commands = StateTransformer.commandsBetween(peer1Snapshot, peer2Snapshot)

    // Should have exactly 1 command: Create file2.txt
    commands.size shouldBe 1
    commands.head shouldBe a[CreateCommand]
    commands.head.asInstanceOf[CreateCommand].location shouldBe "file2.txt"

  test("commandsBetween should generate Update commands for modified files"):
    val oldState = LocationState("file1.txt", "Old content".getBytes)
    val oldSnapshot = Snapshot(oldState)

    val newState = LocationState("file1.txt", "New content".getBytes)
    val newSnapshot = Snapshot(newState)

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    commands.size shouldBe 1
    commands.head shouldBe a[UpdateCommand]

  test("commandsBetween should generate nothing for identical snapshots"):
    val state = LocationState("file1.txt", "Content".getBytes)
    val snapshot1 = Snapshot(state)
    val snapshot2 = Snapshot(state)

    val commands = StateTransformer.commandsBetween(snapshot1, snapshot2)

    commands shouldBe empty

  test("Two peers with different files should merge without deletes"):
    // Peer 1: file1.txt, file2.txt
    val p1State1 = LocationState("file1.txt", "Content 1".getBytes)
    val p1State2 = LocationState("file2.txt", "Content 2".getBytes)
    val peer1Snapshot = Snapshot.empty.withState(p1State1).withState(p1State2)

    // Peer 2: file3.txt, file4.txt
    val p2State1 = LocationState("file3.txt", "Content 3".getBytes)
    val p2State2 = LocationState("file4.txt", "Content 4".getBytes)
    val peer2Snapshot = Snapshot.empty.withState(p2State1).withState(p2State2)

    // Peer 1 receives peer 2's snapshot
    val commandsForPeer1 = StateTransformer.commandsBetween(peer1Snapshot, peer2Snapshot)

    // Should only create file3.txt and file4.txt, NOT delete file1.txt and file2.txt
    commandsForPeer1.size shouldBe 2
    commandsForPeer1.foreach(_ shouldBe a[CreateCommand])
    commandsForPeer1.map(_.asInstanceOf[CreateCommand].location).toSet shouldBe Set("file3.txt", "file4.txt")
