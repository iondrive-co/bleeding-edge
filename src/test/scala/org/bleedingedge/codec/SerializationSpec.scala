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

package org.bleedingedge.codec

import org.bleedingedge.domain.{LocationState, MessageType, ResourceId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, DataInputStream}

class SerializationSpec extends AnyFlatSpec with Matchers:

  "Serialization.statesToBytes" should "serialize empty list to empty array" in {
    val bytes = Serialization.statesToBytes(List.empty)

    bytes.shouldBe(Array.empty[Byte])
  }

  it should "serialize single LocationState" in {
    val state = LocationState("/file1", "content1".getBytes)
    val bytes = Serialization.statesToBytes(List(state))

    bytes.length should be > 0
  }

  it should "serialize multiple LocationStates" in {
    val states = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file3", "content3".getBytes)
    )

    val bytes = Serialization.statesToBytes(states)

    bytes.length should be > 0
    // Should be concatenation of all states
    bytes.length.shouldBe(states.map(LocationState.toBytes).map(_.length).sum)
  }

  "Serialization.bytesToStates" should "deserialize empty array to empty list" in {
    val states = Serialization.bytesToStates(Array.empty[Byte])

    states.shouldBe(List.empty)
  }

  it should "deserialize single state (tail-recursive)" in {
    val originalState = LocationState("/file1", "content1".getBytes)
    val bytes = LocationState.toBytes(originalState)

    val states = Serialization.bytesToStates(bytes)

    states.size.shouldBe(1)
    states.head.location.shouldBe("/file1")
    states.head.resourceId.originalLength.shouldBe("content1".getBytes.length)
  }

  it should "deserialize multiple states (tail-recursive)" in {
    val originalStates = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file3", "content3".getBytes)
    )

    val bytes = Serialization.statesToBytes(originalStates)
    val deserializedStates = Serialization.bytesToStates(bytes)

    deserializedStates.size.shouldBe(3)
    deserializedStates.map(_.location).shouldBe(List("/file1", "/file2", "/file3"))
  }

  it should "not cause stack overflow with many states" in {
    // Test tail recursion by using many states
    val manyStates = (1 to 100).map { i =>
      LocationState(s"/file$i", s"content$i".getBytes)
    }.toList

    val bytes = Serialization.statesToBytes(manyStates)
    val deserializedStates = Serialization.bytesToStates(bytes)

    deserializedStates.size.shouldBe(100)
    deserializedStates.head.location.shouldBe("/file1")
    deserializedStates.last.location.shouldBe("/file100")
  }

  "Serialization round-trip" should "preserve state data" in {
    val originalState = LocationState("/test/file.txt", "test content".getBytes)

    val bytes = Serialization.statesToBytes(List(originalState))
    val roundTrip = Serialization.bytesToStates(bytes)

    roundTrip.size.shouldBe(1)
    roundTrip.head.location.shouldBe(originalState.location)
    roundTrip.head.resourceId.originalLength.shouldBe(originalState.resourceId.originalLength)
    roundTrip.head.resourceId.resourceHash.shouldBe(originalState.resourceId.resourceHash)
  }

  it should "preserve data for multiple states" in {
    val originalStates = List(
      LocationState("/path/file1", "data1".getBytes),
      LocationState("/path/file2", "data2".getBytes)
    )

    val bytes = Serialization.statesToBytes(originalStates)
    val roundTrip = Serialization.bytesToStates(bytes)

    roundTrip.size.shouldBe(2)
    roundTrip.zip(originalStates).foreach { case (deserialized, original) =>
      deserialized.location.shouldBe(original.location)
      deserialized.resourceId.shouldBe(original.resourceId)
    }
  }

  "Serialization.createStateRequest" should "create valid request message" in {
    val hostname = "test-host"
    val hash = "abc123"
    val length = 1024

    val bytes = Serialization.createStateRequest(hostname, hash, length)

    bytes.length should be > 0

    // Verify message format
    val inStream = DataInputStream(ByteArrayInputStream(bytes))
    val messageType = inStream.readShort()
    val readHostname = inStream.readUTF()
    val readLength = inStream.readInt()
    val readHash = inStream.readUTF()

    messageType.shouldBe(MessageType.StateRequest.ordinal.toShort)
    readHostname.shouldBe(hostname)
    readLength.shouldBe(length)
    readHash.shouldBe(hash)
  }

  it should "handle empty hostname and hash" in {
    val bytes = Serialization.createStateRequest("", "", 0)

    bytes.length should be > 0

    val inStream = DataInputStream(ByteArrayInputStream(bytes))
    val messageType = inStream.readShort()
    val hostname = inStream.readUTF()
    val length = inStream.readInt()
    val hash = inStream.readUTF()

    messageType.shouldBe(MessageType.StateRequest.ordinal.toShort)
    hostname.shouldBe("")
    length.shouldBe(0)
    hash.shouldBe("")
  }

  "Serialization.estimateSize" should "return 0 for empty list" in {
    val size = Serialization.estimateSize(List.empty)

    size.shouldBe(0)
  }

  it should "estimate size for single state" in {
    val state = LocationState("/file", "content".getBytes)
    val estimatedSize = Serialization.estimateSize(List(state))

    estimatedSize should be > 0
    // Should be approximately the size of the serialized state
    val actualSize = LocationState.toBytes(state).length
    // Estimate should be close to actual (within reasonable bounds)
    estimatedSize should be > (actualSize - 50)
    estimatedSize should be < (actualSize + 50)
  }

  it should "sum sizes for multiple states" in {
    val states = List(
      LocationState("/file1", "content1".getBytes),
      LocationState("/file2", "content2".getBytes),
      LocationState("/file3", "content3".getBytes)
    )

    val estimatedTotal = Serialization.estimateSize(states)
    val individualEstimates = states.map(s => Serialization.estimateSize(List(s)))

    estimatedTotal.shouldBe(individualEstimates.sum)
  }

  "Serialization.bytesToState" should "deserialize state at offset 0" in {
    val originalState = LocationState("/test", "data".getBytes)
    val bytes = LocationState.toBytes(originalState)

    val deserializedState = Serialization.bytesToState(bytes, 0)

    deserializedState.location.shouldBe(originalState.location)
    deserializedState.resourceId.shouldBe(originalState.resourceId)
  }

  "Serialization immutability" should "not modify original states during serialization" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)
    val originalStates = List(state1, state2)

    val bytes = Serialization.statesToBytes(originalStates)

    // Original states should be unchanged
    originalStates.size.shouldBe(2)
    originalStates.head.location.shouldBe("/file1")
    originalStates(1).location.shouldBe("/file2")
  }

  it should "create independent state instances during deserialization" in {
    val originalState = LocationState("/file", "data".getBytes)
    val bytes = Serialization.statesToBytes(List(originalState))

    val deserialized1 = Serialization.bytesToStates(bytes)
    val deserialized2 = Serialization.bytesToStates(bytes)

    // Should produce equal but separate instances
    deserialized1.head.location.shouldBe(deserialized2.head.location)
    deserialized1.head.resourceId.shouldBe(deserialized2.head.resourceId)
  }
