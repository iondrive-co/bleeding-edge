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

class ResourceIdSpec extends AnyFlatSpec with Matchers:

  "ResourceId.fromBytes" should "create a ResourceId with correct hash and length" in {
    val bytes = "Hello, World!".getBytes
    val resourceId = ResourceId.fromBytes(bytes)

    resourceId.originalLength shouldBe 13
    resourceId.resourceHash should not be empty
  }

  it should "create consistent hashes for identical content" in {
    val bytes1 = "test content".getBytes
    val bytes2 = "test content".getBytes

    val id1 = ResourceId.fromBytes(bytes1)
    val id2 = ResourceId.fromBytes(bytes2)

    id1 shouldBe id2
    id1.resourceHash shouldBe id2.resourceHash
  }

  it should "create different hashes for different content" in {
    val bytes1 = "content A".getBytes
    val bytes2 = "content B".getBytes

    val id1 = ResourceId.fromBytes(bytes1)
    val id2 = ResourceId.fromBytes(bytes2)

    id1 should not be id2
    id1.resourceHash should not be id2.resourceHash
  }

  it should "handle empty byte arrays" in {
    val resourceId = ResourceId.fromBytes(Array.empty)

    resourceId.originalLength shouldBe 0
    resourceId.isEmpty shouldBe true
  }

  it should "throw exception for null bytes" in {
    assertThrows[IllegalArgumentException] {
      ResourceId.fromBytes(null)
    }
  }

  "ResourceId.empty" should "represent an empty resource" in {
    ResourceId.empty.isEmpty shouldBe true
    ResourceId.empty.nonEmpty shouldBe false
    ResourceId.empty.originalLength shouldBe 0
  }

  "ResourceId.isEmpty" should "return true for zero-length resources" in {
    val empty = ResourceId("", 0)
    empty.isEmpty shouldBe true
    empty.nonEmpty shouldBe false
  }

  it should "return false for non-zero length resources" in {
    val nonEmpty = ResourceId("abc123", 100)
    nonEmpty.isEmpty shouldBe false
    nonEmpty.nonEmpty shouldBe true
  }

  "ResourceId equality" should "work correctly" in {
    val id1 = ResourceId("hash123", 100)
    val id2 = ResourceId("hash123", 100)
    val id3 = ResourceId("hash456", 100)
    val id4 = ResourceId("hash123", 200)

    id1 shouldBe id2
    id1 should not be id3
    id1 should not be id4
  }

  "ResourceId hashCode" should "be consistent with equals" in {
    val id1 = ResourceId("hash123", 100)
    val id2 = ResourceId("hash123", 100)

    id1.hashCode shouldBe id2.hashCode
  }
