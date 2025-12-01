package org.bleedingedge.resource

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.domain.LocationState
import org.bleedingedge.infrastructure.scheduling.Scheduler

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
 * Features:
 * - Initial directory scanning
 * - Continuous change monitoring
 * - Recursive directory watching
 * - Automatic cleanup
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

  given scala.concurrent.ExecutionContext = scheduler.executionContext

  /**
   * Performs an initial scan of a directory and returns all files as LocationStates.
   *
   * This method scans the directory tree and creates LocationState objects for all
   * regular files found. It does not start monitoring - use scanAndMonitorDirectory
   * for continuous monitoring.
   *
   * @param basePath The directory to scan
   * @param excludePattern Optional pattern to exclude files/directories (e.g., ".bleedingedge")
   * @return List of LocationStates representing all files in the directory
   */
  def scanDirectory(
    basePath: Path,
    excludePattern: String = ".bleedingedge"
  ): Try[List[LocationState]] = Try {
    import scala.jdk.StreamConverters.*

    logger.info(s"Scanning directory: $basePath")

    if !Files.exists(basePath) then
      logger.warn(s"Directory does not exist: $basePath")
      List.empty
    else if !Files.isDirectory(basePath) then
      logger.warn(s"Path is not a directory: $basePath")
      List.empty
    else
      val states = Files.walk(basePath)
        .toScala(LazyList)
        .filter(p => Files.isRegularFile(p))
        .filter(p => !p.toString.contains(excludePattern))
        .flatMap { filePath =>
          Try {
            val relativePath = basePath.relativize(filePath).toString
            val content = Files.readAllBytes(filePath)
            LocationState(relativePath, content)
          }.toOption
        }
        .toList

      logger.info(s"Scanned ${states.size} files from $basePath")
      states
  }

  /**
   * Performs an initial scan and then monitors for changes.
   *
   * Calls the onChange callback with each file from the initial scan, then
   * continues calling it for any detected changes.
   *
   * @param basePath The directory to scan and monitor
   * @param onChange Callback invoked for each file state (initial + changes)
   * @param excludePattern Optional pattern to exclude files/directories
   * @return Future that completes when monitoring stops
   */
  def scanAndMonitorDirectory(
    basePath: Path,
    onChange: LocationState => Unit,
    excludePattern: String = ".bleedingedge"
  ): Future[Unit] = Future {
    // Perform initial scan
    scanDirectory(basePath, excludePattern) match
      case scala.util.Success(states) =>
        logger.info(s"Reporting ${states.size} initial files")
        states.foreach(onChange)

      case scala.util.Failure(e) =>
        logger.error(s"Failed to scan directory: ${e.getMessage}", e)

    // Start monitoring for changes (blocks until stopped)
    scanDirectoryForChanges(basePath, onChange).value.get.get
  }

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
