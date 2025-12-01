package org.bleedingedge.domain

/**
 * Represents a file system operation command as pure data.
 *
 * All commands are immutable and thread-safe. Commands do not execute themselves;
 * they are executed by CommandExecutor in the Resource layer.
 *
 * @since 2.0.0
 */
sealed trait Command

/**
 * Command to move a file from one location to another.
 *
 * @param from Source location path
 * @param to Destination location path
 */
case class MoveCommand(from: String, to: String) extends Command

/**
 * Command to delete a file at the specified location.
 *
 * @param location The location path to delete
 */
case class DeleteCommand(location: String) extends Command

/**
 * Command to create a new file with specified content.
 *
 * @param location The location path for the new file
 * @param byteLookup Function to retrieve the file contents
 */
case class CreateCommand(location: String, byteLookup: () => Array[Byte]) extends Command

/**
 * Command to update an existing file with new content.
 *
 * @param location The location path of the file to update
 * @param byteLookup Function to retrieve the new file contents
 */
case class UpdateCommand(location: String, byteLookup: () => Array[Byte]) extends Command

/**
 * Command that performs no operation.
 *
 * Used when no file system changes are needed.
 */
case object DoNothingCommand extends Command

object Command:

  /**
   * Factory method to determine the appropriate command based on state changes.
   *
   * Compares an earlier and later LocationState to determine what file system
   * operation is needed.
   *
   * @param earlier The previous state
   * @param later The new state
   * @return The Command that should be executed
   */
  def apply(earlier: LocationState, later: LocationState): Command =
    // Handle empty states first
    val earlierEmpty = earlier.location.isEmpty
    val laterEmpty = later.location.isEmpty

    if earlierEmpty && laterEmpty then
      DoNothingCommand  // Both empty
    else if earlierEmpty then
      // No earlier state, so this is a creation
      CreateCommand(later.location, later.byteLookup.getOrElse(() => Array.empty))
    else if laterEmpty then
      // No later state, so this is a deletion
      DeleteCommand(earlier.location)
    else
      // Both states exist, compare them
      val resourceIdEqual = earlier.resourceId == later.resourceId
      val locationsEqual = earlier.location == later.location

      if resourceIdEqual then
        if locationsEqual then
          DoNothingCommand  // No change
        else
          MoveCommand(earlier.location, later.location)
      else if locationsEqual then
        if earlier.resourceId.originalLength == 0 then
          CreateCommand(later.location, later.byteLookup.getOrElse(() => Array.empty))
        else if later.resourceId.originalLength == 0 then
          DeleteCommand(earlier.location)
        else
          UpdateCommand(later.location, later.byteLookup.getOrElse(() => Array.empty))
      else
        DoNothingCommand  // Not related
