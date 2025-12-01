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

package org.bleedingedge.integration

import org.bleedingedge.infrastructure.daemon.DaemonManager
import org.bleedingedge.infrastructure.resilience.*
import org.bleedingedge.infrastructure.scheduling.Scheduler
import org.bleedingedge.network.NetworkManager
import org.bleedingedge.sync.SyncManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

/**
 * Integration tests for daemon mode with full system lifecycle.
 *
 * Tests daemon PID file management, status checking, and integration with other components.
 */
class DaemonIntegrationSpec extends AnyFlatSpec with Matchers:

  "DaemonManager" should "write and read PID files" in {
    val tempPidFile = Files.createTempFile("daemon-test-", ".pid")

    try
      // Write PID file
      val writeResult = DaemonManager.writePidFile(tempPidFile)
      writeResult.isRight shouldBe true

      // Verify PID file exists
      Files.exists(tempPidFile) shouldBe true

      // Read PID file
      val readResult = DaemonManager.readPidFile(tempPidFile)
      readResult.isRight shouldBe true

      val pid = readResult.toOption.get
      pid shouldBe DaemonManager.currentPid

    finally
      Try(Files.deleteIfExists(tempPidFile))
  }

  it should "detect if daemon is running" in {
    val tempPidFile = Files.createTempFile("daemon-running-", ".pid")

    try
      // Write PID file for current process
      DaemonManager.writePidFile(tempPidFile)

      // Check if daemon is running (should be true for current process)
      DaemonManager.isDaemonRunning(tempPidFile) shouldBe true

    finally
      Try(Files.deleteIfExists(tempPidFile))
  }

  it should "handle non-existent PID file" in {
    val nonExistentFile = Paths.get("/tmp/nonexistent-daemon.pid")

    // Should return false for non-existent PID file
    DaemonManager.isDaemonRunning(nonExistentFile) shouldBe false

    // Reading should return error
    val readResult = DaemonManager.readPidFile(nonExistentFile)
    readResult.isLeft shouldBe true
    readResult.left.toOption.get should include("does not exist")
  }

  it should "get current process ID" in {
    val pid = DaemonManager.currentPid

    pid should be > 0L
    DaemonManager.isProcessRunning(pid) shouldBe true
  }

  "Daemon with components" should "integrate with network and sync managers" in {
    val tempPidFile = Files.createTempFile("daemon-components-", ".pid")
    val tempSyncDir = Files.createTempDirectory("daemon-sync-")

    try
      val scheduler = Scheduler(2)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        val syncManager = SyncManager(tempSyncDir, scheduler)
        val networkManager = new NetworkManager(0, scheduler, (_, _) => ())

        // Write PID file
        DaemonManager.writePidFile(tempPidFile).isRight shouldBe true

        // Start components
        Await.result(networkManager.start(enableDiscovery = false), 5.seconds)
        Await.result(syncManager.start(), 5.seconds)

        // Verify components are running
        networkManager.isRunning shouldBe true
        syncManager.isRunning shouldBe true

        // Verify PID file shows daemon running
        DaemonManager.isDaemonRunning(tempPidFile) shouldBe true

        // Cleanup components
        syncManager.stop()
        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Try(Files.deleteIfExists(tempPidFile))
      Try {
        Files.walk(tempSyncDir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)
      }
  }

  "Daemon with resilience patterns" should "integrate circuit breaker" in {
    val tempPidFile = Files.createTempFile("daemon-resilience-", ".pid")
    val tempSyncDir = Files.createTempDirectory("daemon-resilience-sync-")

    try
      val scheduler = Scheduler(2)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        val syncManager = SyncManager(tempSyncDir, scheduler)
        val networkManager = new NetworkManager(0, scheduler, (_, _) => ())

        // Create circuit breaker for network operations
        val circuitBreaker = new CircuitBreaker(
          failureThreshold = 3,
          resetTimeout = 5.seconds
        )

        // Write PID file
        DaemonManager.writePidFile(tempPidFile)

        // Start components
        Await.result(networkManager.start(enableDiscovery = false), 5.seconds)
        Await.result(syncManager.start(), 5.seconds)

        // Verify circuit is initially closed
        circuitBreaker.currentState shouldBe CircuitState.Closed

        // Simulate network operations through circuit breaker
        val result = circuitBreaker.execute {
          networkManager.isRunning
        }

        result.isSuccess shouldBe true
        result.get shouldBe true

        // Cleanup
        syncManager.stop()
        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Try(Files.deleteIfExists(tempPidFile))
      Try {
        Files.walk(tempSyncDir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)
      }
  }

  it should "integrate health checks with daemon monitoring" in {
    val tempPidFile = Files.createTempFile("daemon-health-", ".pid")
    val tempSyncDir = Files.createTempDirectory("daemon-health-sync-")

    try
      val scheduler = Scheduler(2)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        val syncManager = SyncManager(tempSyncDir, scheduler)
        val networkManager = new NetworkManager(0, scheduler, (_, _) => ())

        // Write PID file
        DaemonManager.writePidFile(tempPidFile)

        // Start components
        Await.result(networkManager.start(enableDiscovery = false), 5.seconds)
        Await.result(syncManager.start(), 5.seconds)

        // Create health checks for all components
        val pingCheck = new PingHealthCheck()
        val networkCheck = new FunctionalHealthCheck(
          "network",
          checkFn = () => networkManager.isRunning
        )
        val syncCheck = new FunctionalHealthCheck(
          "sync",
          checkFn = () => syncManager.isRunning
        )
        val daemonCheck = new FunctionalHealthCheck(
          "daemon",
          checkFn = () => DaemonManager.isDaemonRunning(tempPidFile)
        )
        val memoryCheck = new MemoryHealthCheck()
        val diskCheck = new DiskSpaceHealthCheck(tempSyncDir)

        val composite = new CompositeHealthCheck(List(
          pingCheck,
          networkCheck,
          syncCheck,
          daemonCheck,
          memoryCheck,
          diskCheck
        ))

        // Check overall health
        val healthResult = composite.check()

        healthResult.status should (
          be(HealthStatus.Healthy) or be(HealthStatus.Degraded)
        )

        healthResult.details should contain key "ping"
        healthResult.details should contain key "network"
        healthResult.details should contain key "sync"
        healthResult.details should contain key "daemon"
        healthResult.details should contain key "memory"
        healthResult.details should contain key "disk-space"

        // Verify individual components are healthy
        healthResult.details("network") shouldBe "Healthy"
        healthResult.details("sync") shouldBe "Healthy"
        healthResult.details("daemon") shouldBe "Healthy"

        // Cleanup
        syncManager.stop()
        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Try(Files.deleteIfExists(tempPidFile))
      Try {
        Files.walk(tempSyncDir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)
      }
  }

  it should "use retry policy for writing PID file" in {
    val tempPidFile = Files.createTempFile("daemon-retry-", ".pid")

    try
      val retryPolicy = RetryPolicy(
        maxAttempts = 3,
        initialDelay = 10.millis,
        maxDelay = 100.millis
      )

      // Retry writing PID file
      val result = retryPolicy.retry {
        DaemonManager.writePidFile(tempPidFile) match
          case Right(_) => "success"
          case Left(error) => throw new RuntimeException(error)
      }

      result.isSuccess shouldBe true
      Files.exists(tempPidFile) shouldBe true

    finally
      Try(Files.deleteIfExists(tempPidFile))
  }

  "Daemon with full stack" should "run complete application lifecycle" in {
    val tempPidFile = Files.createTempFile("daemon-fullstack-", ".pid")
    val tempSyncDir = Files.createTempDirectory("daemon-fullstack-sync-")

    try
      val scheduler = Scheduler(4)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        // Create all components
        val syncManager = SyncManager(tempSyncDir, scheduler)
        val networkManager = new NetworkManager(0, scheduler, (_, _) => ())

        // Write PID file
        DaemonManager.writePidFile(tempPidFile)

        // Create resilience components
        val circuitBreaker = new CircuitBreaker(
          failureThreshold = 5,
          resetTimeout = 30.seconds
        )

        val retryPolicy = RetryPolicy.networkDefault

        val healthChecks = new CompositeHealthCheck(List(
          new PingHealthCheck(),
          new FunctionalHealthCheck("network", () => networkManager.isRunning),
          new FunctionalHealthCheck("sync", () => syncManager.isRunning),
          new FunctionalHealthCheck("daemon", () => DaemonManager.isDaemonRunning(tempPidFile)),
          new MemoryHealthCheck()
        ))

        // Start components with retry
        val networkStartResult = retryPolicy.retry {
          Await.result(networkManager.start(enableDiscovery = false), 5.seconds)
        }
        networkStartResult.isSuccess shouldBe true

        val syncStartResult = retryPolicy.retry {
          Await.result(syncManager.start(), 5.seconds)
        }
        syncStartResult.isSuccess shouldBe true

        // Verify health
        val health = healthChecks.check()
        health.isHealthy shouldBe true

        // Verify circuit breaker works with network operations
        val networkOp = circuitBreaker.execute {
          networkManager.isRunning
        }
        networkOp.isSuccess shouldBe true
        networkOp.get shouldBe true

        // Check daemon status
        DaemonManager.isDaemonRunning(tempPidFile) shouldBe true

        // Cleanup
        syncManager.stop()
        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Try(Files.deleteIfExists(tempPidFile))
      Try {
        Files.walk(tempSyncDir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)
      }
  }
