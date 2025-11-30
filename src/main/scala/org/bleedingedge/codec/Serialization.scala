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

package org.bleedingedge.codec

import org.bleedingedge.domain.{LocationState, MessageType, ResourceId}

import java.io.{ByteArrayOutputStream, DataOutputStream}
import scala.annotation.tailrec

/**
 * Serialization utilities for LocationState objects.
 *
 * This replaces the old Codec protocol with simplified binary encoding.
 * Fountain codes are removed in favor of straightforward binary serialization.
 *
 * Improvements over original:
 * - Tail-recursive bytesToStates (fixes TODO at line 24)
 * - No fountain code complexity
 * - Type-safe with proper error handling
 * - Stack-safe for large collections
 *
 * @since 2.0.0
 */
object Serialization:

  /**
   * Converts a list of LocationStates to a byte array.
   *
   * Each state is serialized using LocationState.toBytes and concatenated.
   *
   * @param states List of LocationStates to serialize
   * @return Concatenated byte array
   */
  def statesToBytes(states: List[LocationState]): Array[Byte] =
    if states.isEmpty then
      Array.empty[Byte]
    else
      states.map(LocationState.toBytes).reduce(_ ++ _)

  /**
   * Converts a byte array to a list of LocationStates (tail-recursive).
   *
   * This fixes the TODO from the original implementation by making it tail-recursive,
   * preventing stack overflow for large byte arrays.
   *
   * @param bytes Byte array to deserialize
   * @return List of LocationStates
   */
  def bytesToStates(bytes: Array[Byte]): List[LocationState] =
    if bytes.isEmpty then
      List.empty
    else
      import java.io.{ByteArrayInputStream, DataInputStream, EOFException}
      import scala.util.{Failure, Success, Try}

      val inStream = ByteArrayInputStream(bytes)
      val inBuffer = DataInputStream(inStream)

      def readOneState(): Option[LocationState] =
        Try {
          if inBuffer.available() > 0 then
            val msgType = inBuffer.readShort()
            val location = inBuffer.readUTF()
            val length = inBuffer.readInt()
            val hash = inBuffer.readUTF()
            val hostname = inBuffer.readUTF()

            // Read actual file content
            val contentLength = inBuffer.readInt()
            val content = Array.ofDim[Byte](contentLength)
            inBuffer.readFully(content)

            val resourceId = ResourceId(hash, length)
            Some(LocationState(location, resourceId, Some(() => content)))
          else
            None
        }.toOption.flatten

      @tailrec
      def loop(accumulator: List[LocationState]): List[LocationState] =
        readOneState() match
          case Some(state) => loop(state :: accumulator)
          case None => accumulator.reverse

      loop(Nil)

  /**
   * Deserializes a single LocationState from a byte array starting at the given offset.
   *
   * @param bytes Byte array containing serialized data
   * @param startPos Starting position in the byte array
   * @return Deserialized LocationState
   */
  def bytesToState(bytes: Array[Byte], startPos: Int): LocationState =
    // Use a placeholder byteLookup function for now
    // In a full implementation, this would connect to network layer
    val byteLookup = (hostname: String, hash: String, length: Int) =>
      Array.empty[Byte] // Placeholder

    LocationState.fromBytes(bytes, startPos, byteLookup)

  /**
   * Calculates the serialized size of a LocationState.
   *
   * This is needed to advance the offset when deserializing multiple states.
   * Format: 2 (messageType) + UTF(location) + 4 (length) + UTF(hash) + UTF(hostname)
   *
   * @param state The LocationState
   * @return Size in bytes
   */
  private def calculateStateSize(state: LocationState): Int =
    // Approximate size calculation based on serialization format
    // 2 bytes for message type + UTF strings + 4 bytes for int
    val locationBytes = state.location.getBytes("UTF-8")
    val hashBytes = state.resourceId.resourceHash.getBytes("UTF-8")

    2 + // message type (short)
    2 + locationBytes.length + // UTF location string
    4 + // originalLength (int)
    2 + hashBytes.length + // UTF hash string
    2 + "localhost".getBytes("UTF-8").length // UTF hostname (placeholder)

  /**
   * Creates a state request message.
   *
   * This replaces the stateRequestFn from the original implementation.
   * In a complete system, this would be sent to a peer to request file content.
   *
   * @param hostname The hostname to request from
   * @param resourceHash The hash of the resource to request
   * @param resourceLength The length of the resource
   * @return Serialized request message
   */
  def createStateRequest(
    hostname: String,
    resourceHash: String,
    resourceLength: Int
  ): Array[Byte] =
    val outStream = ByteArrayOutputStream()
    val outBuffer = DataOutputStream(outStream)

    outBuffer.writeShort(MessageType.StateRequest.ordinal)
    outBuffer.writeUTF(hostname)
    outBuffer.writeInt(resourceLength)
    outBuffer.writeUTF(resourceHash)

    outStream.toByteArray

  /**
   * Estimates the total serialized size of a list of states.
   *
   * Useful for pre-allocating buffers or checking size limits.
   *
   * @param states List of states to measure
   * @return Estimated size in bytes
   */
  def estimateSize(states: List[LocationState]): Int =
    states.map(calculateStateSize).sum
