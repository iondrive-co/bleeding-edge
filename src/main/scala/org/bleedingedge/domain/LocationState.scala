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

import com.typesafe.scalalogging.LazyLogging

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.net.InetAddress

/**
 * Identifies the 'where' and 'what' of a monitored byte array for later comparison.
 *
 * This class is fully immutable and thread-safe.
 *
 * @param location The location identifier (e.g., a filesystem path)
 * @param resourceId Unique identifier for the resource content
 * @param byteLookup Optional function to retrieve the actual bytes when needed
 * @since 2.0.0
 */
case class LocationState(
  location: String,
  resourceId: ResourceId,
  byteLookup: Option[() => Array[Byte]] = None
) extends LazyLogging:

  require(location != null, "LocationState initialized with null location")

  logger.debug(s"Constructed $location state, ${resourceId.originalLength} bytes with hash ${resourceId.resourceHash}")

  /**
   * Returns true if this LocationState represents an empty location.
   */
  def isEmpty: Boolean = resourceId.isEmpty

  /**
   * Returns true if this LocationState represents a non-empty location.
   */
  def nonEmpty: Boolean = resourceId.nonEmpty

  /**
   * Retrieves the bytes for this location using the byteLookup function.
   * @return The bytes if byteLookup is defined, otherwise an empty array
   */
  def getBytes: Array[Byte] = byteLookup.map(_()).getOrElse(Array.empty)

object LocationState:

  /**
   * Creates an empty LocationState (used to represent no data).
   */
  lazy val empty: LocationState = LocationState("", ResourceId.empty, None)

  /**
   * Creates a LocationState with just a location (no content).
   */
  def apply(location: String): LocationState =
    LocationState(location, ResourceId.empty, None)

  /**
   * Creates a LocationState from a location and its byte content.
   */
  def apply(location: String, bytes: Array[Byte]): LocationState =
    val resourceId = ResourceId.fromBytes(bytes)
    LocationState(location, resourceId, Some(() => bytes))

  /**
   * Deserializes a LocationState from a byte array.
   *
   * @param bytes The serialized bytes
   * @param startReadPos Starting position in the byte array
   * @param byteLookup Function to retrieve bytes given (hostname, hash, length)
   * @return The deserialized LocationState
   */
  def fromBytes(
    bytes: Array[Byte],
    startReadPos: Int,
    byteLookup: (String, String, Int) => Array[Byte]
  ): LocationState =
    val inStream = ByteArrayInputStream(bytes, startReadPos, bytes.length)
    val inBuffer = DataInputStream(inStream)

    val msgType = inBuffer.readShort()
    val location = inBuffer.readUTF()
    val length = inBuffer.readInt()
    val hash = inBuffer.readUTF()
    val hostname = inBuffer.readUTF()

    val resourceId = ResourceId(hash, length)
    val lookup = () => byteLookup(hostname, hash, length)

    LocationState(location, resourceId, Some(lookup))

  /**
   * Serializes a LocationState to a byte array.
   *
   * Includes both metadata AND actual file content for complete state transfer.
   *
   * @param state The LocationState to serialize
   * @return The serialized bytes
   */
  def toBytes(state: LocationState): Array[Byte] =
    val outStream = ByteArrayOutputStream()
    val outBuffer = DataOutputStream(outStream)

    outBuffer.writeShort(MessageType.StateBroadcast.ordinal)
    outBuffer.writeUTF(state.location)
    outBuffer.writeInt(state.resourceId.originalLength)
    outBuffer.writeUTF(state.resourceId.resourceHash)
    outBuffer.writeUTF(InetAddress.getLocalHost.getHostName)

    // Include actual file content
    val content = state.getBytes
    outBuffer.writeInt(content.length)
    outBuffer.write(content)

    outStream.toByteArray
