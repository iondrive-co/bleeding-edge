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

package org.bleedingedge.transposition

import org.bleedingedge.domain.{Command, DoNothingCommand, LocationState, Snapshot}

/**
 * Pure functional state transformation utilities.
 *
 * This replaces the old Transposition protocol with immutable, pure functional operations.
 * No mutable state is used - all functions return new values.
 *
 * Improvements over original:
 * - No mutable Snapshot state (addresses TODO at line 20)
 * - Pure functional design - all operations are side-effect free
 * - Explicit snapshot parameters instead of implicit global state
 * - Type-safe and composable
 *
 * @since 2.0.0
 */
object StateTransformer:

  /**
   * Accumulates a LocationState into a snapshot, detecting snapshot boundaries.
   *
   * When a LocationState for an existing location is encountered, it signals the
   * completion of the current snapshot and the start of a new one.
   *
   * This replaces the mutable packChange function with a pure functional approach.
   *
   * @param currentSnapshot The snapshot being accumulated
   * @param newState The new LocationState to add
   * @return (updatedSnapshot, completedSnapshot)
   *         - updatedSnapshot: The snapshot to continue accumulating into
   *         - completedSnapshot: Some(snapshot) if a snapshot boundary was detected, None otherwise
   */
  def accumulateState(
    currentSnapshot: Snapshot,
    newState: LocationState
  ): (Snapshot, Option[Snapshot]) =
    if currentSnapshot.contains(newState.location) then
      // Location already exists - current snapshot is complete, start new one
      val newSnapshot = Snapshot(newState)
      (newSnapshot, Some(currentSnapshot))
    else
      // Add to current snapshot
      val updatedSnapshot = currentSnapshot.withState(newState)
      (updatedSnapshot, None)

  /**
   * Computes the sequence of Commands needed to merge a remote snapshot into local state.
   *
   * This implements MERGE behavior for P2P synchronization:
   * - CreateCommand for files that exist remotely but not locally
   * - UpdateCommand for files that exist in both but with different content
   * - NO DeleteCommand - local files are preserved even if not present remotely
   *
   * This is appropriate for P2P file sync where peers should merge their states,
   * not mirror each other. Each peer contributes files to the shared pool.
   *
   * @param localSnapshot The current local state
   * @param remoteSnapshot The remote peer's state
   * @return Sequence of Commands to execute locally
   */
  def commandsBetween(localSnapshot: Snapshot, remoteSnapshot: Snapshot): Seq[Command] =
    val localLocations = localSnapshot.locationStates
    val remoteLocations = remoteSnapshot.locationStates

    // Only consider files that exist remotely (merge, don't delete)
    remoteLocations.map { case (location, remoteState) =>
      val localState = localLocations.getOrElse(location, LocationState.empty)
      Command(localState, remoteState)
    }.toSeq.filterNot(_.isInstanceOf[DoNothingCommand.type])

  /**
   * Folds a sequence of LocationStates into snapshots.
   *
   * This is useful for processing a stream of location states and detecting
   * snapshot boundaries automatically.
   *
   * @param states Sequence of LocationStates
   * @return List of completed Snapshots
   */
  def statesToSnapshots(states: Seq[LocationState]): List[Snapshot] =
    val (finalSnapshot, completedSnapshots) = states.foldLeft(
      (Snapshot.empty, List.empty[Snapshot])
    ) { case ((currentSnapshot, completed), state) =>
      val (updatedSnapshot, maybeCompleted) = accumulateState(currentSnapshot, state)
      val newCompleted = maybeCompleted.map(_ :: completed).getOrElse(completed)
      (updatedSnapshot, newCompleted)
    }

    // Add final snapshot if non-empty
    val allSnapshots = if finalSnapshot.nonEmpty then
      finalSnapshot :: completedSnapshots
    else
      completedSnapshots

    allSnapshots.reverse

  /**
   * Computes the diff between two snapshots as a map of operations.
   *
   * @param oldSnapshot The previous state
   * @param newSnapshot The desired state
   * @return Map of location -> Command
   */
  def diff(oldSnapshot: Snapshot, newSnapshot: Snapshot): Map[String, Command] =
    val oldLocations = oldSnapshot.locationStates
    val newLocations = newSnapshot.locationStates
    val allLocations = oldLocations.keySet ++ newLocations.keySet

    allLocations.map { location =>
      val oldState = oldLocations.getOrElse(location, LocationState.empty)
      val newState = newLocations.getOrElse(location, LocationState.empty)
      location -> Command(oldState, newState)
    }.toMap.filter { case (_, cmd) =>
      !cmd.isInstanceOf[DoNothingCommand.type]
    }
