package org.bleedingedge.resource

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.domain.*
import org.bleedingedge.infrastructure.resilience.RetryPolicy

import java.nio.file.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * Executes filesystem commands from the domain layer.
 *
 * This class separates command execution from command definition, keeping
 * domain objects pure while centralizing all filesystem operations in the
 * Resource layer.
 *
 * Features:
 * - Idempotent operations
 * - Race condition handling
 * - Retry logic for transient failures
 * - Thread-safe execution
 *
 * @param basePath The base directory for resolving relative paths
 * @param retryPolicy Optional retry policy for filesystem operations
 * @since 2.0.0
 */
class CommandExecutor(
  val basePath: Path,
  retryPolicy: Option[RetryPolicy] = Some(RetryPolicy.conservative)
)(using ec: ExecutionContext) extends LazyLogging:

  /**
   * Executes a single command synchronously.
   *
   * @param command The command to execute
   * @return Try indicating success or failure
   */
  def execute(command: Command): Try[Unit] =
    retryPolicy match
      case Some(policy) => policy.retry(executeInternal(command))
      case None => Try(executeInternal(command))

  /**
   * Executes a single command asynchronously.
   *
   * @param command The command to execute
   * @return Future completing when execution finishes
   */
  def executeAsync(command: Command): Future[Unit] =
    Future {
      execute(command).get
    }

  /**
   * Executes multiple commands synchronously.
   *
   * @param commands List of commands to execute
   * @return List of Try results, one per command
   */
  def executeAll(commands: List[Command]): List[Try[Unit]] =
    commands.map(execute)

  /**
   * Executes multiple commands asynchronously.
   *
   * @param commands List of commands to execute
   * @return Future completing when all commands finish
   */
  def executeAllAsync(commands: List[Command]): Future[List[Try[Unit]]] =
    Future.sequence(commands.map(cmd => Future(execute(cmd))))

  /**
   * Internal command execution logic.
   */
  private def executeInternal(command: Command): Unit =
    command match
      case MoveCommand(from, to) =>
        executeMoveCommand(from, to)

      case DeleteCommand(location) =>
        executeDeleteCommand(location)

      case CreateCommand(location, byteLookup) =>
        executeCreateCommand(location, byteLookup)

      case UpdateCommand(location, byteLookup) =>
        executeUpdateCommand(location, byteLookup)

      case DoNothingCommand =>
        () // No-op

  /**
   * Executes a move operation.
   */
  private def executeMoveCommand(from: String, to: String): Unit =
    val localFrom = basePath.resolve(from)
    val localTo = basePath.resolve(to)

    Files.createDirectories(localTo.getParent)
    Files.move(localFrom, localTo, StandardCopyOption.REPLACE_EXISTING)

    logger.debug(s"Moved file: $from -> $to")

  /**
   * Executes a delete operation.
   */
  private def executeDeleteCommand(location: String): Unit =
    val path = basePath.resolve(location)
    val deleted = Files.deleteIfExists(path)

    if deleted then
      logger.debug(s"Deleted file: $location")
    else
      logger.trace(s"File already deleted: $location")

  /**
   * Executes a create operation with race condition handling.
   */
  private def executeCreateCommand(location: String, byteLookup: () => Array[Byte]): Unit =
    val path = basePath.resolve(location)
    val newContent = byteLookup()

    // Ensure parent directories exist
    Option(path.getParent).foreach(Files.createDirectories(_))

    // Try to create the file, handling race conditions
    try
      Files.write(path, newContent, StandardOpenOption.CREATE_NEW)
      logger.debug(s"Created file: $location")
    catch
      case _: FileAlreadyExistsException =>
        // File was created between our check and write - verify content matches
        val existingContent = Files.readAllBytes(path)
        if !java.util.Arrays.equals(existingContent, newContent) then
          // File exists with different content - this should be an UpdateCommand
          throw java.io.IOException(s"File $location already exists with different content")
        else
          logger.trace(s"File already exists with same content: $location")

  /**
   * Executes an update operation with idempotent behavior.
   */
  private def executeUpdateCommand(location: String, byteLookup: () => Array[Byte]): Unit =
    val path = basePath.resolve(location)
    val newContent = byteLookup()

    // Ensure parent directories exist
    Option(path.getParent).foreach(Files.createDirectories(_))

    // Idempotent: if file already has this content, do nothing
    if Files.exists(path) then
      val existingContent = Files.readAllBytes(path)
      if !java.util.Arrays.equals(existingContent, newContent) then
        // Content differs - update it (use TRUNCATE_EXISTING instead of delete+create)
        Files.write(path, newContent, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        logger.debug(s"Updated file: $location")
      else
        logger.trace(s"File already has correct content: $location")
    else
      // File doesn't exist - create it, handling race conditions
      try
        Files.write(path, newContent, StandardOpenOption.CREATE_NEW)
        logger.debug(s"Created file (via update): $location")
      catch
        case _: FileAlreadyExistsException =>
          // File was created between our check and write - update it instead
          Files.write(path, newContent, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
          logger.debug(s"Updated file (after race): $location")

object CommandExecutor:

  /**
   * Creates a CommandExecutor with default retry policy.
   */
  def apply(basePath: Path)(using ExecutionContext): CommandExecutor =
    new CommandExecutor(basePath, Some(RetryPolicy.conservative))

  /**
   * Creates a CommandExecutor without retry policy.
   */
  def withoutRetry(basePath: Path)(using ExecutionContext): CommandExecutor =
    new CommandExecutor(basePath, None)
