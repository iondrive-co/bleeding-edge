package org.bleedingedge.domain

import com.typesafe.scalalogging.LazyLogging

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
