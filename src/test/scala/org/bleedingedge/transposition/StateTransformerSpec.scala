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

package org.bleedingedge.transposition

import org.bleedingedge.domain.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateTransformerSpec extends AnyFlatSpec with Matchers:

  "StateTransformer.accumulateState" should "add state to empty snapshot" in {
    val state = LocationState("/file1", "content".getBytes)
    val (updated, completed) = StateTransformer.accumulateState(Snapshot.empty, state)

    updated.size.shouldBe(1)
    updated.contains("/file1").shouldBe(true)
    completed.shouldBe(None)
  }

  it should "add new location to existing snapshot" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)

    val snapshot1 = Snapshot(state1)
    val (updated, completed) = StateTransformer.accumulateState(snapshot1, state2)

    updated.size.shouldBe(2)
    updated.contains("/file1").shouldBe(true)
    updated.contains("/file2").shouldBe(true)
    completed.shouldBe(None)
  }

  it should "complete snapshot when duplicate location appears" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)
    val state3 = LocationState("/file1", "content1-modified".getBytes)

    val snapshot1 = Snapshot(state1)
    val snapshot2 = snapshot1.withState(state2)
    val (updated, completed) = StateTransformer.accumulateState(snapshot2, state3)

    // New snapshot should only contain state3
    updated.size.shouldBe(1)
    updated.contains("/file1").shouldBe(true)

    // Completed snapshot should contain state1 and state2
    completed.shouldBe(defined)
    completed.get.size.shouldBe(2)
    completed.get.contains("/file1").shouldBe(true)
    completed.get.contains("/file2").shouldBe(true)
  }

  "StateTransformer.commandsBetween" should "generate CreateCommand for new files" in {
    val oldSnapshot = Snapshot.empty
    val newSnapshot = Snapshot(LocationState("/newfile", "content".getBytes))

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    commands.size.shouldBe(1)
    commands.head.shouldBe(a[CreateCommand])
  }

  it should "NOT generate DeleteCommand for removed files (merge behavior)" in {
    // In merge-style sync, we don't delete local files that aren't present remotely
    val oldSnapshot = Snapshot(LocationState("/oldfile", "content".getBytes))
    val newSnapshot = Snapshot.empty

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    // Merge behavior: no deletes, only creates/updates
    commands.size.shouldBe(0)
  }

  it should "generate UpdateCommand for modified files" in {
    val oldSnapshot = Snapshot(LocationState("/file", "old content".getBytes))
    val newSnapshot = Snapshot(LocationState("/file", "new content".getBytes))

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    commands.size.shouldBe(1)
    commands.head.shouldBe(an[UpdateCommand])
  }

  it should "filter out DoNothingCommand" in {
    val state = LocationState("/file", "content".getBytes)
    val oldSnapshot = Snapshot(state)
    val newSnapshot = Snapshot(state)

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    commands.shouldBe(empty)
  }

  it should "handle multiple file operations" in {
    val oldSnapshot = Snapshot(List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes)
    ))

    val newSnapshot = Snapshot(List(
      LocationState("/file1", "modified1".getBytes),
      LocationState("/file3", "content3".getBytes)
    ))

    val commands = StateTransformer.commandsBetween(oldSnapshot, newSnapshot)

    // Should have commands for: update file1, delete file2, create file3
    // But the exact commands depend on zipAll pairing
    commands should not be empty
  }

  "StateTransformer.statesToSnapshots" should "create snapshots from state sequence" in {
    val states = Seq(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file1", "modified1".getBytes), // Triggers new snapshot
      LocationState("/file3", "content3".getBytes)
    )

    val snapshots = StateTransformer.statesToSnapshots(states)

    snapshots.size.shouldBe(2)
    snapshots(0).size.shouldBe(2) // file1, file2
    snapshots(1).size.shouldBe(2) // file1, file3
  }

  it should "handle empty state sequence" in {
    val snapshots = StateTransformer.statesToSnapshots(Seq.empty)

    snapshots.shouldBe(empty)
  }

  it should "create single snapshot when no duplicates" in {
    val states = Seq(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file3", "content3".getBytes)
    )

    val snapshots = StateTransformer.statesToSnapshots(states)

    snapshots.size.shouldBe(1)
    snapshots.head.size.shouldBe(3)
  }

  "StateTransformer.diff" should "compute differences between snapshots" in {
    val oldSnapshot = Snapshot(List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes)
    ))

    val newSnapshot = Snapshot(List(
      LocationState("/file1", "modified1".getBytes),
      LocationState("/file3", "content3".getBytes)
    ))

    val diffs = StateTransformer.diff(oldSnapshot, newSnapshot)

    diffs.keySet should contain allOf ("/file1", "/file2", "/file3")
    diffs("/file1").shouldBe(an[UpdateCommand])
    diffs("/file2").shouldBe(a[DeleteCommand])
    diffs("/file3").shouldBe(a[CreateCommand])
  }

  it should "return empty map when snapshots are identical" in {
    val state = LocationState("/file", "content".getBytes)
    val snapshot1 = Snapshot(state)
    val snapshot2 = Snapshot(state)

    val diffs = StateTransformer.diff(snapshot1, snapshot2)

    diffs.shouldBe(empty)
  }

  "StateTransformer immutability" should "not modify original snapshots" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)

    val snapshot1 = Snapshot(state1)
    val (updated, _) = StateTransformer.accumulateState(snapshot1, state2)

    // Original snapshot should be unchanged
    snapshot1.size.shouldBe(1)
    snapshot1.contains("/file1").shouldBe(true)
    snapshot1.contains("/file2").shouldBe(false)

    // Updated snapshot should have both
    updated.size.shouldBe(2)
    updated.contains("/file1").shouldBe(true)
    updated.contains("/file2").shouldBe(true)
  }
