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

import java.nio.file.*
import scala.util.Try

/**
 * Represents a file system operation command.
 *
 * All commands are immutable and thread-safe.
 *
 * @since 2.0.0
 */
sealed trait Command:

  /**
   * Executes this command against the file system.
   *
   * @param dirBase The base directory for resolving relative paths
   * @return A Try indicating success or failure
   */
  def apply(dirBase: Path): Try[Unit]

  /**
   * Helper to construct a path from parent and child components.
   */
  protected def makePath(parent: Path, child: String): Path =
    // TODO: match common root recursively (Paths.get(from).getParent.equals(Paths.get(to).getParent))
    parent.resolve(child)

/**
 * Command to move a file from one location to another.
 *
 * BUG FIX: The original implementation at line 32 incorrectly used 'from' twice.
 * This has been corrected to properly use 'to' for the destination path.
 *
 * @param from Source location path
 * @param to Destination location path
 */
case class MoveCommand(from: String, to: String) extends Command:

  override def apply(dirBase: Path): Try[Unit] = Try {
    val localFrom = makePath(dirBase, from)
    val localTo = makePath(dirBase, to)  // BUG FIX: was makePath(dirBase, from)
    Files.createDirectories(localTo)
    Files.move(localFrom, localTo.resolve(localFrom.getFileName), StandardCopyOption.REPLACE_EXISTING)
  }

/**
 * Command to delete a file at the specified location.
 *
 * @param location The location path to delete
 */
case class DeleteCommand(location: String) extends Command:

  override def apply(dirBase: Path): Try[Unit] = Try {
    Files.deleteIfExists(makePath(dirBase, location))
  }

/**
 * Command to create a new file with specified content.
 *
 * @param location The location path for the new file
 * @param byteLookup Function to retrieve the file contents
 */
case class CreateCommand(location: String, byteLookup: () => Array[Byte]) extends Command:

  override def apply(dirBase: Path): Try[Unit] = Try {
    val path = makePath(dirBase, location)
    val newContent = byteLookup()

    // Ensure parent directories exist
    Option(path.getParent).foreach(Files.createDirectories(_))

    // Try to create the file, handling race conditions
    try
      Files.write(path, newContent, StandardOpenOption.CREATE_NEW)
    catch
      case _: FileAlreadyExistsException =>
        // File was created between our check and write - verify content matches
        val existingContent = Files.readAllBytes(path)
        if !java.util.Arrays.equals(existingContent, newContent) then
          // File exists with different content - this should be an UpdateCommand
          throw new java.io.IOException(s"File $location already exists with different content")
        // else: File already exists with same content - success!
  }

/**
 * Command to update an existing file with new content.
 *
 * @param location The location path of the file to update
 * @param byteLookup Function to retrieve the new file contents
 */
case class UpdateCommand(location: String, byteLookup: () => Array[Byte]) extends Command:

  override def apply(dirBase: Path): Try[Unit] = Try {
    val path = makePath(dirBase, location)
    val newContent = byteLookup()

    // Ensure parent directories exist
    Option(path.getParent).foreach(Files.createDirectories(_))

    // Idempotent: if file already has this content, do nothing
    if Files.exists(path) then
      val existingContent = Files.readAllBytes(path)
      if !java.util.Arrays.equals(existingContent, newContent) then
        // Content differs - update it (use TRUNCATE_EXISTING instead of delete+create)
        Files.write(path, newContent, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
      // else: already has correct content, do nothing
    else
      // File doesn't exist - create it, handling race conditions
      try
        Files.write(path, newContent, StandardOpenOption.CREATE_NEW)
      catch
        case _: FileAlreadyExistsException =>
          // File was created between our check and write - update it instead
          Files.write(path, newContent, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
  }

/**
 * Command that performs no operation.
 *
 * Used when no file system changes are needed.
 */
case object DoNothingCommand extends Command:

  override def apply(dirBase: Path): Try[Unit] = Try(())

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
