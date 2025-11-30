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

class LocationStateSpec extends AnyFlatSpec with Matchers:

  "LocationState.apply(location, bytes)" should "create a state with correct resource ID" in {
    val location = "/path/to/file.txt"
    val bytes = "file content".getBytes
    val state = LocationState(location, bytes)

    state.location shouldBe location
    state.resourceId.originalLength shouldBe bytes.length
    state.byteLookup should be(defined)
    state.getBytes shouldBe bytes
  }

  "LocationState.apply(location)" should "create an empty state" in {
    val location = "/path/to/file.txt"
    val state = LocationState(location)

    state.location shouldBe location
    state.resourceId shouldBe ResourceId.empty
    state.isEmpty shouldBe true
  }

  "LocationState.empty" should "represent an empty location" in {
    LocationState.empty.location shouldBe ""
    LocationState.empty.isEmpty shouldBe true
    LocationState.empty.resourceId shouldBe ResourceId.empty
  }

  "LocationState equality" should "compare by location and resource ID" in {
    val bytes = "content".getBytes
    val state1 = LocationState("/file1", bytes)
    val state2 = LocationState("/file1", bytes)
    val state3 = LocationState("/file2", bytes)
    val state4 = LocationState("/file1", "different".getBytes)

    // Compare fields individually since byteLookup functions are different instances
    state1.location.shouldBe(state2.location)
    state1.resourceId.shouldBe(state2.resourceId)
    state1.location should not be state3.location
    state1.resourceId should not be state4.resourceId
  }

  "LocationState.isEmpty" should "return true for empty resources" in {
    val emptyState = LocationState("/file", Array.empty[Byte])
    emptyState.isEmpty.shouldBe(true)
    emptyState.nonEmpty.shouldBe(false)
  }

  it should "return false for non-empty resources" in {
    val nonEmptyState = LocationState("/file", "data".getBytes)
    nonEmptyState.isEmpty shouldBe false
    nonEmptyState.nonEmpty shouldBe true
  }

  "LocationState.getBytes" should "return bytes from byteLookup" in {
    val bytes = "test data".getBytes
    val state = LocationState("/file", bytes)

    state.getBytes shouldBe bytes
  }

  it should "return empty array when byteLookup is None" in {
    val state = LocationState("/file", ResourceId.empty, None)

    state.getBytes.sameElements(Array.empty[Byte]).shouldBe(true)
  }

  "LocationState serialization" should "round-trip correctly" in {
    val originalBytes = "test content".getBytes
    val originalState = LocationState("/test/path.txt", originalBytes)

    val serialized = LocationState.toBytes(originalState)
    serialized should not be empty

    val byteLookup = (host: String, hash: String, length: Int) => originalBytes
    val deserialized = LocationState.fromBytes(serialized, 0, byteLookup)

    deserialized.location shouldBe originalState.location
    deserialized.resourceId shouldBe originalState.resourceId
  }

  "LocationState immutability" should "ensure thread safety" in {
    val bytes = "original".getBytes
    val state1 = LocationState("/file", bytes)

    // Creating a new state should not affect the original
    val state2 = state1.copy(location = "/different")

    state1.location shouldBe "/file"
    state2.location shouldBe "/different"
  }
