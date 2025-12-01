package org.bleedingedge.codec

import org.bleedingedge.domain.{LocationState, MessageType, ResourceId}
import org.bleedingedge.network.NetworkMessage

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.util.UUID
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
   * Each state is serialized and concatenated.
   *
   * @param states List of LocationStates to serialize
   * @return Concatenated byte array
   */
  def statesToBytes(states: List[LocationState]): Array[Byte] =
    if states.isEmpty then
      Array.empty[Byte]
    else
      states.map(encodeLocationState).reduce(_ ++ _)

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
      import java.io.{ByteArrayInputStream, DataInputStream}
      import scala.util.Try

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

    decodeLocationState(bytes, startPos, byteLookup)

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

  // ========== NetworkMessage Serialization ==========

  /**
   * Serializes a NetworkMessage to bytes.
   *
   * @param message The NetworkMessage to serialize
   * @return Serialized bytes
   */
  def encodeNetworkMessage(message: NetworkMessage): Array[Byte] =
    val outStream = ByteArrayOutputStream()
    val out = DataOutputStream(outStream)

    message match
      case NetworkMessage.Hello(peerId, hostname) =>
        out.writeByte(0)
        out.writeLong(peerId.getMostSignificantBits)
        out.writeLong(peerId.getLeastSignificantBits)
        out.writeUTF(hostname)

      case NetworkMessage.Goodbye(peerId) =>
        out.writeByte(1)
        out.writeLong(peerId.getMostSignificantBits)
        out.writeLong(peerId.getLeastSignificantBits)

      case NetworkMessage.StateBroadcast(states) =>
        out.writeByte(2)
        out.writeInt(states.length)
        out.write(states)

      case NetworkMessage.StateRequest(hostname, resourceHash, resourceLength) =>
        out.writeByte(3)
        out.writeUTF(hostname)
        out.writeUTF(resourceHash)
        out.writeInt(resourceLength)

      case NetworkMessage.Heartbeat(timestamp) =>
        out.writeByte(4)
        out.writeLong(timestamp)

      case NetworkMessage.Acknowledgment(messageId) =>
        out.writeByte(5)
        out.writeLong(messageId.getMostSignificantBits)
        out.writeLong(messageId.getLeastSignificantBits)

    out.flush()
    outStream.toByteArray

  /**
   * Deserializes a NetworkMessage from bytes.
   *
   * @param bytes The bytes to deserialize
   * @return Deserialized NetworkMessage
   */
  def decodeNetworkMessage(bytes: Array[Byte]): NetworkMessage =
    val in = DataInputStream(ByteArrayInputStream(bytes))

    val messageType = in.readByte()
    messageType match
      case 0 => // Hello
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val peerId = UUID(mostSig, leastSig)
        val hostname = in.readUTF()
        NetworkMessage.Hello(peerId, hostname)

      case 1 => // Goodbye
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val peerId = UUID(mostSig, leastSig)
        NetworkMessage.Goodbye(peerId)

      case 2 => // StateBroadcast
        val length = in.readInt()
        val states = Array.ofDim[Byte](length)
        in.readFully(states)
        NetworkMessage.StateBroadcast(states)

      case 3 => // StateRequest
        val hostname = in.readUTF()
        val resourceHash = in.readUTF()
        val resourceLength = in.readInt()
        NetworkMessage.StateRequest(hostname, resourceHash, resourceLength)

      case 4 => // Heartbeat
        val timestamp = in.readLong()
        NetworkMessage.Heartbeat(timestamp)

      case 5 => // Acknowledgment
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val messageId = UUID(mostSig, leastSig)
        NetworkMessage.Acknowledgment(messageId)

      case unknown =>
        throw java.io.IOException(s"Unknown message type: $unknown")

  /**
   * Reads a NetworkMessage from an existing DataInputStream.
   * Used for continuous streaming deserialization.
   *
   * @param in The DataInputStream to read from
   * @return Deserialized NetworkMessage
   */
  def readNetworkMessage(in: DataInputStream): NetworkMessage =
    val messageType = in.readByte()
    messageType match
      case 0 => // Hello
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val peerId = UUID(mostSig, leastSig)
        val hostname = in.readUTF()
        NetworkMessage.Hello(peerId, hostname)

      case 1 => // Goodbye
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val peerId = UUID(mostSig, leastSig)
        NetworkMessage.Goodbye(peerId)

      case 2 => // StateBroadcast
        val length = in.readInt()
        val states = Array.ofDim[Byte](length)
        in.readFully(states)
        NetworkMessage.StateBroadcast(states)

      case 3 => // StateRequest
        val hostname = in.readUTF()
        val resourceHash = in.readUTF()
        val resourceLength = in.readInt()
        NetworkMessage.StateRequest(hostname, resourceHash, resourceLength)

      case 4 => // Heartbeat
        val timestamp = in.readLong()
        NetworkMessage.Heartbeat(timestamp)

      case 5 => // Acknowledgment
        val mostSig = in.readLong()
        val leastSig = in.readLong()
        val messageId = UUID(mostSig, leastSig)
        NetworkMessage.Acknowledgment(messageId)

      case unknown =>
        throw java.io.IOException(s"Unknown message type: $unknown")

  /**
   * Writes a NetworkMessage to an existing DataOutputStream.
   * Used for continuous streaming serialization.
   *
   * @param out The DataOutputStream to write to
   * @param message The NetworkMessage to write
   */
  def writeNetworkMessage(out: DataOutputStream, message: NetworkMessage): Unit =
    message match
      case NetworkMessage.Hello(peerId, hostname) =>
        out.writeByte(0)
        out.writeLong(peerId.getMostSignificantBits)
        out.writeLong(peerId.getLeastSignificantBits)
        out.writeUTF(hostname)

      case NetworkMessage.Goodbye(peerId) =>
        out.writeByte(1)
        out.writeLong(peerId.getMostSignificantBits)
        out.writeLong(peerId.getLeastSignificantBits)

      case NetworkMessage.StateBroadcast(states) =>
        out.writeByte(2)
        out.writeInt(states.length)
        out.write(states)

      case NetworkMessage.StateRequest(hostname, resourceHash, resourceLength) =>
        out.writeByte(3)
        out.writeUTF(hostname)
        out.writeUTF(resourceHash)
        out.writeInt(resourceLength)

      case NetworkMessage.Heartbeat(timestamp) =>
        out.writeByte(4)
        out.writeLong(timestamp)

      case NetworkMessage.Acknowledgment(messageId) =>
        out.writeByte(5)
        out.writeLong(messageId.getMostSignificantBits)
        out.writeLong(messageId.getLeastSignificantBits)

    out.flush()

  // ========== LocationState Serialization (moved from Domain) ==========

  /**
   * Serializes a single LocationState to bytes.
   * Moved from LocationState companion object to centralize all codec logic.
   *
   * @param state The LocationState to serialize
   * @return Serialized bytes
   */
  def encodeLocationState(state: LocationState): Array[Byte] =
    val outStream = ByteArrayOutputStream()
    val outBuffer = DataOutputStream(outStream)

    outBuffer.writeShort(MessageType.StateBroadcast.ordinal)
    outBuffer.writeUTF(state.location)
    outBuffer.writeInt(state.resourceId.originalLength)
    outBuffer.writeUTF(state.resourceId.resourceHash)
    outBuffer.writeUTF(java.net.InetAddress.getLocalHost.getHostName)

    // Include actual file content
    val content = state.getBytes
    outBuffer.writeInt(content.length)
    outBuffer.write(content)

    outStream.toByteArray

  /**
   * Deserializes a single LocationState from bytes.
   * Moved from LocationState companion object to centralize all codec logic.
   *
   * @param bytes The serialized bytes
   * @param startReadPos Starting position in the byte array
   * @param byteLookup Function to retrieve bytes given (hostname, hash, length)
   * @return Deserialized LocationState
   */
  def decodeLocationState(
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
