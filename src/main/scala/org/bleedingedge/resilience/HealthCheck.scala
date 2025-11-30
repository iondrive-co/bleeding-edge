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

package org.bleedingedge.resilience

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Health check status.
 */
enum HealthStatus:
  case Healthy
  case Degraded
  case Unhealthy

/**
 * Result of a health check.
 *
 * @param name Name of the health check
 * @param status Health status
 * @param message Optional message with details
 * @param details Optional additional details
 */
case class HealthCheckResult(
  name: String,
  status: HealthStatus,
  message: Option[String] = None,
  details: Map[String, String] = Map.empty
):
  def isHealthy: Boolean = status == HealthStatus.Healthy
  def isDegraded: Boolean = status == HealthStatus.Degraded
  def isUnhealthy: Boolean = status == HealthStatus.Unhealthy

/**
 * Trait for health check implementations.
 */
trait HealthCheck:
  /**
   * Name of this health check.
   */
  def name: String

  /**
   * Performs the health check.
   *
   * @return Health check result
   */
  def check(): HealthCheckResult

  /**
   * Performs the health check asynchronously.
   *
   * @param ec Execution context
   * @return Future of health check result
   */
  def checkAsync()(using ec: ExecutionContext): Future[HealthCheckResult] =
    Future(check())

/**
 * Composite health check that aggregates multiple health checks.
 *
 * @param checks Individual health checks to aggregate
 */
class CompositeHealthCheck(val checks: List[HealthCheck]) extends HealthCheck with LazyLogging:

  override def name: String = "composite"

  override def check(): HealthCheckResult =
    val results = checks.map(_.check())

    // Overall status is the worst individual status
    val overallStatus = results.map(_.status).maxBy {
      case HealthStatus.Unhealthy => 2
      case HealthStatus.Degraded => 1
      case HealthStatus.Healthy => 0
    }

    val unhealthyCount = results.count(_.isUnhealthy)
    val degradedCount = results.count(_.isDegraded)
    val healthyCount = results.count(_.isHealthy)

    val message = s"$healthyCount healthy, $degradedCount degraded, $unhealthyCount unhealthy"

    HealthCheckResult(
      name = name,
      status = overallStatus,
      message = Some(message),
      details = results.map(r => r.name -> r.status.toString).toMap
    )

  override def checkAsync()(using ec: ExecutionContext): Future[HealthCheckResult] =
    Future.sequence(checks.map(_.checkAsync())).map { results =>
      // Same aggregation logic as synchronous check
      val overallStatus = results.map(_.status).maxBy {
        case HealthStatus.Unhealthy => 2
        case HealthStatus.Degraded => 1
        case HealthStatus.Healthy => 0
      }

      val unhealthyCount = results.count(_.isUnhealthy)
      val degradedCount = results.count(_.isDegraded)
      val healthyCount = results.count(_.isHealthy)

      val message = s"$healthyCount healthy, $degradedCount degraded, $unhealthyCount unhealthy"

      HealthCheckResult(
        name = name,
        status = overallStatus,
        message = Some(message),
        details = results.map(r => r.name -> r.status.toString).toMap
      )
    }

/**
 * Simple health check that executes a function.
 *
 * @param checkName Name of the health check
 * @param checkFn Function that returns true if healthy
 * @param degradedFn Optional function that returns true if degraded
 */
class FunctionalHealthCheck(
  val checkName: String,
  checkFn: () => Boolean,
  degradedFn: Option[() => Boolean] = None
) extends HealthCheck:

  override def name: String = checkName

  override def check(): HealthCheckResult =
    Try {
      if checkFn() then
        HealthCheckResult(name, HealthStatus.Healthy, Some("OK"))
      else
        degradedFn match
          case Some(fn) if fn() =>
            HealthCheckResult(name, HealthStatus.Degraded, Some("Service degraded"))
          case _ =>
            HealthCheckResult(name, HealthStatus.Unhealthy, Some("Check failed"))
    } match
      case Success(result) => result
      case Failure(e) =>
        HealthCheckResult(
          name,
          HealthStatus.Unhealthy,
          Some(s"Check threw exception: ${e.getMessage}")
        )

/**
 * Ping-based health check.
 *
 * Simple health check that always returns healthy.
 * Useful for basic liveness probes.
 */
class PingHealthCheck extends HealthCheck:
  override def name: String = "ping"

  override def check(): HealthCheckResult =
    HealthCheckResult(name, HealthStatus.Healthy, Some("pong"))

/**
 * Disk space health check.
 *
 * @param path Path to check disk space
 * @param minFreeBytes Minimum free bytes for healthy status
 * @param minFreeBytesWarning Minimum free bytes for degraded status
 */
class DiskSpaceHealthCheck(
  path: java.nio.file.Path,
  minFreeBytes: Long = 1024 * 1024 * 1024, // 1 GB
  minFreeBytesWarning: Long = 5L * 1024 * 1024 * 1024 // 5 GB
) extends HealthCheck with LazyLogging:

  override def name: String = "disk-space"

  override def check(): HealthCheckResult =
    Try {
      val fileStore = java.nio.file.Files.getFileStore(path)
      val usableSpace = fileStore.getUsableSpace

      val status = if usableSpace >= minFreeBytesWarning then
        HealthStatus.Healthy
      else if usableSpace >= minFreeBytes then
        HealthStatus.Degraded
      else
        HealthStatus.Unhealthy

      val usableGB = usableSpace / (1024.0 * 1024.0 * 1024.0)
      val message = f"$usableGB%.2f GB free"

      HealthCheckResult(
        name = name,
        status = status,
        message = Some(message),
        details = Map(
          "usableBytes" -> usableSpace.toString,
          "path" -> path.toString
        )
      )
    } match
      case Success(result) => result
      case Failure(e) =>
        logger.error(s"Failed to check disk space for $path", e)
        HealthCheckResult(
          name,
          HealthStatus.Unhealthy,
          Some(s"Error: ${e.getMessage}")
        )

/**
 * Memory health check.
 *
 * @param maxUsedMemoryPercent Maximum used memory percentage for healthy status
 * @param maxUsedMemoryPercentWarning Maximum used memory percentage for degraded status
 */
class MemoryHealthCheck(
  maxUsedMemoryPercent: Double = 90.0,
  maxUsedMemoryPercentWarning: Double = 75.0
) extends HealthCheck:

  override def name: String = "memory"

  override def check(): HealthCheckResult =
    val runtime = Runtime.getRuntime
    val maxMemory = runtime.maxMemory()
    val totalMemory = runtime.totalMemory()
    val freeMemory = runtime.freeMemory()
    val usedMemory = totalMemory - freeMemory
    val usedPercent = (usedMemory.toDouble / maxMemory.toDouble) * 100.0

    val status = if usedPercent < maxUsedMemoryPercentWarning then
      HealthStatus.Healthy
    else if usedPercent < maxUsedMemoryPercent then
      HealthStatus.Degraded
    else
      HealthStatus.Unhealthy

    val usedMB = usedMemory / (1024 * 1024)
    val maxMB = maxMemory / (1024 * 1024)
    val message = f"$usedMB MB / $maxMB MB ($usedPercent%.1f%%)"

    HealthCheckResult(
      name = name,
      status = status,
      message = Some(message),
      details = Map(
        "usedBytes" -> usedMemory.toString,
        "maxBytes" -> maxMemory.toString,
        "usedPercent" -> f"$usedPercent%.2f"
      )
    )
