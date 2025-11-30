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

package org.bleedingedge.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SnapshotSpec extends AnyFlatSpec with Matchers:

  "Snapshot.empty" should "create an empty snapshot" in {
    Snapshot.empty.isEmpty shouldBe true
    Snapshot.empty.nonEmpty shouldBe false
    Snapshot.empty.size shouldBe 0
    Snapshot.empty.states shouldBe empty
  }

  "Snapshot.withState" should "add a new location state" in {
    val state = LocationState("/file1", "content".getBytes)
    val snapshot = Snapshot.empty.withState(state)

    snapshot.size shouldBe 1
    snapshot.contains("/file1") shouldBe true
    snapshot.get("/file1") shouldBe Some(state)
  }

  it should "update an existing location state" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file1", "content2".getBytes)

    val snapshot1 = Snapshot.empty.withState(state1)
    val snapshot2 = snapshot1.withState(state2)

    snapshot2.size shouldBe 1
    snapshot2.get("/file1") shouldBe Some(state2)
  }

  it should "not modify the original snapshot (immutability)" in {
    val state = LocationState("/file1", "content".getBytes)
    val snapshot1 = Snapshot.empty
    val snapshot2 = snapshot1.withState(state)

    snapshot1.isEmpty shouldBe true
    snapshot2.size shouldBe 1
  }

  "Snapshot.withStates" should "add multiple states" in {
    val states = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file3", "content3".getBytes)
    )

    val snapshot = Snapshot.empty.withStates(states)

    snapshot.size shouldBe 3
    snapshot.contains("/file1") shouldBe true
    snapshot.contains("/file2") shouldBe true
    snapshot.contains("/file3") shouldBe true
  }

  "Snapshot.contains" should "return true for existing locations" in {
    val state = LocationState("/file1", "content".getBytes)
    val snapshot = Snapshot.empty.withState(state)

    snapshot.contains("/file1") shouldBe true
    snapshot.contains("/nonexistent") shouldBe false
  }

  "Snapshot.get" should "return Some for existing locations" in {
    val state = LocationState("/file1", "content".getBytes)
    val snapshot = Snapshot.empty.withState(state)

    snapshot.get("/file1") shouldBe Some(state)
  }

  it should "return None for non-existing locations" in {
    val snapshot = Snapshot.empty

    snapshot.get("/nonexistent") shouldBe None
  }

  "Snapshot.states" should "return all location states as a list" in {
    val states = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes)
    )

    val snapshot = Snapshot.empty.withStates(states)

    snapshot.states should have size 2
    snapshot.states should contain allElementsOf states
  }

  "Snapshot ordering" should "compare by timestamp" in {
    val timestamp1 = 1000L
    val timestamp2 = 2000L
    val snapshot1 = Snapshot(Map.empty, timestamp1)
    val snapshot2 = Snapshot(Map.empty, timestamp2)

    snapshot1.compare(snapshot2) should be < 0
    snapshot2.compare(snapshot1) should be > 0
  }

  "Snapshot construction" should "accept a single LocationState" in {
    val state = LocationState("/file1", "content".getBytes)
    val snapshot = Snapshot(state)

    snapshot.size shouldBe 1
    snapshot.contains("/file1") shouldBe true
  }

  it should "accept multiple LocationStates" in {
    val states = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes)
    )

    val snapshot = Snapshot(states)

    snapshot.size shouldBe 2
    snapshot.contains("/file1") shouldBe true
    snapshot.contains("/file2") shouldBe true
  }

  "Snapshot immutability" should "ensure thread safety" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)

    val snapshot1 = Snapshot(state1)
    val snapshot2 = snapshot1.withState(state2)

    // Original snapshot should be unchanged
    snapshot1.size shouldBe 1
    snapshot1.contains("/file1") shouldBe true
    snapshot1.contains("/file2") shouldBe false

    // New snapshot should have both
    snapshot2.size shouldBe 2
    snapshot2.contains("/file1") shouldBe true
    snapshot2.contains("/file2") shouldBe true
  }

  it should "handle concurrent modifications safely" in {
    // This test demonstrates that immutability makes snapshots thread-safe
    val initialSnapshot = Snapshot.empty
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)

    // Simulating concurrent modifications
    val snapshot1 = initialSnapshot.withState(state1)
    val snapshot2 = initialSnapshot.withState(state2)

    // Both branches should be independent
    snapshot1.size shouldBe 1
    snapshot2.size shouldBe 1
    snapshot1.contains("/file1") shouldBe true
    snapshot1.contains("/file2") shouldBe false
    snapshot2.contains("/file1") shouldBe false
    snapshot2.contains("/file2") shouldBe true
  }
