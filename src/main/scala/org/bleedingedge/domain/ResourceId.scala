/*
 * Copyright (c) 2012, 2025 Miles Hampson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.bleedingedge.domain

import java.math.BigInteger
import java.security.MessageDigest

/**
 * Uniquely identifies the contents of a byte array without storing it.
 *
 * Uses MD5 hash for content identification. While there is an exceedingly small
 * chance of hash collision (around k²/(6.8*10³⁸) percent for k resources), this
 * does not violate the equals contract but means it is theoretically possible
 * that a change may be missed.
 *
 * @param resourceHash MD5 hash of the content as a hexadecimal string
 * @param originalLength Length of the original byte array
 * @since 2.0.0
 */
case class ResourceId(resourceHash: String, originalLength: Int):

  /**
   * Returns true if this ResourceId represents an empty resource.
   */
  def isEmpty: Boolean = originalLength == 0

  /**
   * Returns true if this ResourceId represents a non-empty resource.
   */
  def nonEmpty: Boolean = originalLength > 0

object ResourceId:

  /**
   * Creates a ResourceId from a byte array by computing its MD5 hash.
   *
   * @param bytes The byte array to hash
   * @return A ResourceId representing the content
   */
  def fromBytes(bytes: Array[Byte]): ResourceId =
    require(bytes != null, "Cannot create ResourceId from null bytes")
    val hash = BigInteger(1, MessageDigest.getInstance("MD5").digest(bytes)).toString(16)
    ResourceId(hash, bytes.length)

  /**
   * Singleton representing an empty resource (no bytes).
   */
  val empty: ResourceId = ResourceId("", 0)
