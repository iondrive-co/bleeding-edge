/*
 * Copyright (c) 2025 Miles Hampson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.bleedingedge.daemon

import com.typesafe.scalalogging.LazyLogging

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.{Failure, Success, Try}

/**
 * Manages daemon mode operation including PID files and shutdown hooks.
 *
 * @since 3.0.0
 */
object DaemonManager extends LazyLogging:

  /**
   * Gets the current process ID.
   */
  def currentPid: Long =
    val runtimeName = ManagementFactory.getRuntimeMXBean.getName
    // Runtime name is typically "pid@hostname"
    runtimeName.split("@").headOption match
      case Some(pidStr) => Try(pidStr.toLong).getOrElse(-1L)
      case None => -1L

  /**
   * Writes a PID file.
   *
   * @param pidFile Path to PID file
   * @return Success or error message
   */
  def writePidFile(pidFile: Path): Either[String, Unit] =
    Try {
      val pid = currentPid
      if pid == -1L then
        throw new RuntimeException("Could not determine process ID")

      Files.createDirectories(pidFile.getParent)
      Files.writeString(pidFile, pid.toString)
      logger.info(s"Wrote PID file: $pidFile (PID: $pid)")

      // Add shutdown hook to remove PID file
      val cleanup = new Runnable {
        def run(): Unit =
          Try(Files.deleteIfExists(pidFile))
          logger.info(s"Removed PID file: $pidFile")
      }
      Runtime.getRuntime.addShutdownHook(new Thread(cleanup))

    } match
      case Success(_) => Right(())
      case Failure(e) => Left(s"Failed to write PID file: ${e.getMessage}")

  /**
   * Reads a PID from a PID file.
   *
   * @param pidFile Path to PID file
   * @return PID or error message
   */
  def readPidFile(pidFile: Path): Either[String, Long] =
    if !Files.exists(pidFile) then
      Left(s"PID file does not exist: $pidFile")
    else
      Try {
        Files.readString(pidFile).trim.toLong
      } match
        case Success(pid) => Right(pid)
        case Failure(e) => Left(s"Failed to read PID file: ${e.getMessage}")

  /**
   * Checks if a process with the given PID is running.
   *
   * Note: This is a basic check using ProcessHandle. On some systems,
   * it may not work correctly.
   *
   * @param pid Process ID to check
   * @return True if process appears to be running
   */
  def isProcessRunning(pid: Long): Boolean =
    Try {
      ProcessHandle.of(pid).isPresent
    }.getOrElse(false)

  /**
   * Checks if a daemon is running based on PID file.
   *
   * @param pidFile Path to PID file
   * @return True if daemon appears to be running
   */
  def isDaemonRunning(pidFile: Path): Boolean =
    readPidFile(pidFile) match
      case Right(pid) => isProcessRunning(pid)
      case Left(_) => false

  /**
   * Removes a stale PID file.
   *
   * A PID file is considered stale if the process is not running.
   *
   * @param pidFile Path to PID file
   * @return Success or error message
   */
  def removeIfStale(pidFile: Path): Either[String, Boolean] =
    if !Files.exists(pidFile) then
      Right(false) // No PID file, nothing to remove
    else
      readPidFile(pidFile) match
        case Right(pid) =>
          if isProcessRunning(pid) then
            Left(s"Process $pid is still running")
          else
            Try(Files.deleteIfExists(pidFile)) match
              case Success(deleted) =>
                if deleted then
                  logger.info(s"Removed stale PID file: $pidFile (PID: $pid)")
                Right(deleted)
              case Failure(e) =>
                Left(s"Failed to remove PID file: ${e.getMessage}")

        case Left(error) =>
          Left(error)

  /**
   * Installs a shutdown hook to execute cleanup code.
   *
   * @param cleanup Cleanup function to execute on shutdown
   */
  def installShutdownHook(cleanup: () => Unit): Unit =
    val hook = new Runnable {
      def run(): Unit =
        logger.info("Shutdown hook triggered")
        Try(cleanup()).recover {
          case e: Exception =>
            logger.error("Error during shutdown cleanup", e)
        }
    }
    Runtime.getRuntime.addShutdownHook(new Thread(hook))
