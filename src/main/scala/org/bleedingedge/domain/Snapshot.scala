package org.bleedingedge.domain

/**
 * An immutable snapshot of file system state at a point in time.
 *
 * Represents a collection of LocationStates captured during a single scan cycle.
 * Completely immutable and thread-safe (fixes the mutable shared state issue
 * from the original implementation).
 *
 * @param locationStates Map of location paths to their states
 * @param lastUpdateTime Timestamp when this snapshot was last updated (in nanoseconds)
 * @since 2.0.0
 */
case class Snapshot(
  locationStates: Map[String, LocationState] = Map.empty,
  lastUpdateTime: Long = System.nanoTime()
) extends Ordered[Snapshot]:

  /**
   * Returns all location states as a list.
   */
  def states: List[LocationState] = locationStates.values.toList

  /**
   * Returns the number of locations in this snapshot.
   */
  def size: Int = locationStates.size

  /**
   * Returns true if this snapshot contains no locations.
   */
  def isEmpty: Boolean = locationStates.isEmpty

  /**
   * Returns true if this snapshot contains at least one location.
   */
  def nonEmpty: Boolean = locationStates.nonEmpty

  /**
   * Creates a new Snapshot with an additional or updated LocationState.
   *
   * @param state The LocationState to add or update
   * @return A new Snapshot with the state included
   */
  def withState(state: LocationState): Snapshot =
    copy(
      locationStates = locationStates + (state.location -> state),
      lastUpdateTime = System.nanoTime()
    )

  /**
   * Creates a new Snapshot with multiple LocationStates added or updated.
   *
   * @param states The LocationStates to add or update
   * @return A new Snapshot with all states included
   */
  def withStates(states: Iterable[LocationState]): Snapshot =
    copy(
      locationStates = locationStates ++ states.map(s => s.location -> s),
      lastUpdateTime = System.nanoTime()
    )

  /**
   * Checks if this snapshot contains a location.
   *
   * @param location The location path to check
   * @return True if the location exists in this snapshot
   */
  def contains(location: String): Boolean = locationStates.contains(location)

  /**
   * Gets the LocationState for a specific location.
   *
   * @param location The location path to look up
   * @return An Option containing the LocationState if found
   */
  def get(location: String): Option[LocationState] = locationStates.get(location)

  /**
   * Compares snapshots by their timestamp.
   */
  override def compare(that: Snapshot): Int =
    lastUpdateTime.compareTo(that.lastUpdateTime)

object Snapshot:

  /**
   * An empty snapshot with no location states.
   */
  val empty: Snapshot = Snapshot()

  /**
   * Creates a snapshot from a single LocationState.
   */
  def apply(state: LocationState): Snapshot =
    Snapshot(Map(state.location -> state))

  /**
   * Creates a snapshot from multiple LocationStates.
   */
  def apply(states: Iterable[LocationState]): Snapshot =
    Snapshot(states.map(s => s.location -> s).toMap)
