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

package org.bleedingedge.resource

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.domain.LocationState
import org.bleedingedge.scheduling.Scheduler

import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.file.*
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

/**
 * Monitors a filesystem directory for changes and reports them via callback.
 *
 * This replaces the old Resource protocol with modern, non-blocking file monitoring
 * that uses callbacks instead of Scala Actors.
 *
 * Improvements over original:
 * - Uses scala.jdk.CollectionConverters instead of deprecated JavaConversions
 * - Callback-based instead of Actor-based
 * - Returns cancellable Future instead of blocking
 * - Proper resource management with Using
 * - Thread-safe with immutable collections
 *
 * @param scheduler The scheduler for running the monitoring task
 * @since 2.0.0
 */
class FileSystemMonitor(scheduler: Scheduler) extends LazyLogging:

  /**
   * Scans a directory and monitors it for changes, invoking the callback for each change.
   *
   * @param dirPath The root directory to monitor (includes subdirectories)
   * @param onChange Callback invoked with LocationState for each file change
   * @return Future that completes when monitoring stops
   */
  def scanDirectoryForChanges(
    dirPath: Path,
    onChange: LocationState => Unit
  ): Future[Unit] =
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    scheduler.execute("file-monitor") {
      Using.resource(FileSystems.getDefault.newWatchService()) { watcher =>
        var watchKeys = Map.empty[WatchKey, Path]

        // Initial scan and setup - don't notify changes (snapshot already initialized)
        watchKeys = loadResourcesAt(dirPath, dirPath, watchKeys, watcher, onChange, None, notifyChanges = false)

        logger.info(s"Starting monitoring of $dirPath")

        // Monitor loop
        var monitoring = true
        while monitoring do
          try
            val key = watcher.take() // Blocks until event available
            watchKeys.get(key) match
              case Some(keyPath) =>
                logger.debug(s"Processing events for: $keyPath")

                // Process all events for this key
                for event <- key.pollEvents().asScala do
                  val e = event.asInstanceOf[WatchEvent[Path]]
                  val changedPath = keyPath.resolve(e.context())
                  logger.debug(s"Change detected at: $changedPath")
                  watchKeys = loadResourcesAt(dirPath, changedPath, watchKeys, watcher, onChange, Some(key))

                // Re-queue the key for more events
                if !key.reset() then
                  logger.debug(s"Watch key at $keyPath invalid, removing watch")
                  watchKeys = loadResourcesAt(dirPath, keyPath, watchKeys, watcher, onChange, Some(key))

              case None =>
                logger.warn("Received event for unregistered watch key")

          catch
            case _: InterruptedException =>
              logger.info("File monitoring interrupted, stopping")
              monitoring = false
            case e: Exception =>
              logger.error("Error during file monitoring", e)
              monitoring = false
      }
    }

  /**
   * Loads resources at the specified path and registers watches.
   *
   * @param pathToHandle Path to scan
   * @param currentKeys Current watch key mappings
   * @param watcher Watch service to register with
   * @param onChange Callback for state changes
   * @param existingKey Existing watch key for the path (if any)
   * @return Updated watch key mappings
   */
  private def loadResourcesAt(
    basePath: Path,
    pathToHandle: Path,
    currentKeys: Map[WatchKey, Path],
    watcher: WatchService,
    onChange: LocationState => Unit,
    existingKey: Option[WatchKey],
    notifyChanges: Boolean = true
  ): Map[WatchKey, Path] =
    val fileToHandle = pathToHandle.toFile
    var updatedKeys = currentKeys

    // Handle directories
    if fileToHandle.isDirectory then
      if fileToHandle.exists then
        val containedFiles = Option(fileToHandle.listFiles()).getOrElse(Array.empty[File])
        logger.debug(s"Scanning directory $pathToHandle with ${containedFiles.length} files")

        // Recursively load all contained files
        for file <- containedFiles do
          updatedKeys = loadResourcesAt(basePath, file.toPath, updatedKeys, watcher, onChange, existingKey, notifyChanges)

        // Register watch on this directory
        val key = pathToHandle.register(
          watcher,
          StandardWatchEventKinds.ENTRY_CREATE,
          StandardWatchEventKinds.ENTRY_DELETE,
          StandardWatchEventKinds.ENTRY_MODIFY
        )
        updatedKeys = updatedKeys + (key -> pathToHandle)

    // Handle regular files
    else if fileToHandle.exists then
      logger.debug(s"Found file: $pathToHandle")
      bytesFromFile(fileToHandle) match
        case Some(bytes) =>
          if notifyChanges then
            // Use relative path from base directory
            val relativePath = basePath.relativize(pathToHandle).toString
            onChange(LocationState(relativePath, bytes))
        case None =>
          logger.warn(s"Could not read file: $pathToHandle")

    // Handle deletions
    if !fileToHandle.exists then
      updatedKeys = removeWatch(pathToHandle, updatedKeys, existingKey)
      if notifyChanges then
        // Use relative path from base directory
        val relativePath = basePath.relativize(pathToHandle).toString
        onChange(LocationState(relativePath))

    updatedKeys

  /**
   * Removes a watch for a deleted path.
   *
   * @param pathToHandle Path that was deleted
   * @param currentKeys Current watch key mappings
   * @param existingKey The watch key associated with this path
   * @return Updated watch key mappings
   */
  private def removeWatch(
    pathToHandle: Path,
    currentKeys: Map[WatchKey, Path],
    existingKey: Option[WatchKey]
  ): Map[WatchKey, Path] =
    // Check if this was a watched directory
    if currentKeys.values.exists(_ == pathToHandle) then
      logger.debug(s"Removing watch for deleted directory: $pathToHandle")
      existingKey.map(key => currentKeys - key).getOrElse(currentKeys)
    else
      logger.debug(s"File deleted: $pathToHandle")
      currentKeys

  /**
   * Reads all bytes from a file.
   *
   * @param file The file to read
   * @return Some(bytes) if successful, None if failed
   */
  private def bytesFromFile(file: File): Option[Array[Byte]] =
    Try {
      Using.resource(new FileInputStream(file)) { fis =>
        Using.resource(new BufferedInputStream(fis)) { bis =>
          bis.readAllBytes()
        }
      }
    }.toOption

object FileSystemMonitor:

  /**
   * Creates a FileSystemMonitor with a provided scheduler.
   */
  def apply(scheduler: Scheduler): FileSystemMonitor =
    new FileSystemMonitor(scheduler)
