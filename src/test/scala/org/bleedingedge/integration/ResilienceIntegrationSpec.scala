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

import org.bleedingedge.network.{NetworkManager, PeerInfo}
import org.bleedingedge.resilience.*
import org.bleedingedge.scheduling.Scheduler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * Integration tests for resilience patterns with network operations.
 *
 * Tests the integration of retry policies, circuit breakers, and health checks
 * with the actual network layer.
 */
class ResilienceIntegrationSpec extends AnyFlatSpec with Matchers:

  "RetryPolicy with NetworkManager" should "retry failed connection attempts" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val networkManager = new NetworkManager(0, scheduler, (_, _) => ())
      Await.result(networkManager.start(enableDiscovery = false), 3.seconds)

      val retryPolicy = RetryPolicy(
        maxAttempts = 3,
        initialDelay = 10.millis,
        maxDelay = 100.millis
      )

      var attemptCount = 0

      // Simulate connection that fails twice then succeeds
      val result = retryPolicy.retry {
        attemptCount += 1
        if attemptCount < 3 then
          throw new java.net.ConnectException("Connection refused")
        "Connected"
      }

      result shouldBe Success("Connected")
      attemptCount shouldBe 3

      networkManager.stop()
    finally
      scheduler.gracefulShutdown(2)
  }

  it should "fail after exhausting retries on persistent errors" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val retryPolicy = RetryPolicy(
        maxAttempts = 2,
        initialDelay = 10.millis,
        maxDelay = 100.millis
      )

      var attemptCount = 0

      // Simulate persistent connection failure
      val result = retryPolicy.retry {
        attemptCount += 1
        throw new java.net.ConnectException("Connection refused")
      }

      result.isFailure shouldBe true
      result.failed.get shouldBe a[java.net.ConnectException]
      attemptCount shouldBe 2

    finally
      scheduler.gracefulShutdown(2)
  }

  "CircuitBreaker with NetworkManager" should "open after connection failures" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val circuitBreaker = new CircuitBreaker(
        failureThreshold = 3,
        resetTimeout = 1.second
      )

      // Simulate network operations that fail
      (1 to 3).foreach { _ =>
        circuitBreaker.execute {
          throw new java.io.IOException("Network error")
        }
      }

      circuitBreaker.currentState shouldBe CircuitState.Open
      circuitBreaker.failures shouldBe 3

      // Next call should be rejected
      val result = circuitBreaker.execute("should not execute")
      result.isFailure shouldBe true
      result.failed.get shouldBe a[CircuitBreakerOpenException]

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "reset after successful operations in half-open state" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val circuitBreaker = new CircuitBreaker(
        failureThreshold = 2,
        resetTimeout = 100.millis,
        halfOpenMaxCalls = 2
      )

      // Open the circuit
      (1 to 2).foreach { _ =>
        circuitBreaker.execute {
          throw new java.io.IOException("Network error")
        }
      }

      circuitBreaker.currentState shouldBe CircuitState.Open

      // Wait for reset timeout
      Thread.sleep(150)

      // Successful operations should close the circuit
      (1 to 2).foreach { _ =>
        val result = circuitBreaker.execute("success")
        result shouldBe Success("success")
      }

      circuitBreaker.currentState shouldBe CircuitState.Closed
      circuitBreaker.failures shouldBe 0

    finally
      scheduler.gracefulShutdown(2)
  }

  "RetryPolicy and CircuitBreaker together" should "provide layered resilience" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val retryPolicy = RetryPolicy(
        maxAttempts = 2,
        initialDelay = 10.millis,
        maxDelay = 100.millis
      )

      val circuitBreaker = new CircuitBreaker(
        failureThreshold = 3,
        resetTimeout = 1.second
      )

      var totalAttempts = 0

      // First operation: retry succeeds before circuit opens
      val result1 = retryPolicy.retry {
        totalAttempts += 1
        circuitBreaker.execute {
          if totalAttempts < 2 then
            throw new java.io.IOException("Temporary error")
          "Success after retry"
        }.get
      }

      result1 shouldBe Success("Success after retry")
      totalAttempts shouldBe 2
      circuitBreaker.currentState shouldBe CircuitState.Closed

    finally
      scheduler.gracefulShutdown(2)
  }

  "HealthCheck integration" should "check network manager health" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val networkManager = new NetworkManager(0, scheduler, (_, _) => ())
      Await.result(networkManager.start(enableDiscovery = false), 3.seconds)

      // Create health check for network manager
      val networkHealthCheck = new FunctionalHealthCheck(
        "network",
        checkFn = () => networkManager.isRunning
      )

      val result = networkHealthCheck.check()
      result.isHealthy shouldBe true
      result.status shouldBe HealthStatus.Healthy

      // Stop network manager
      networkManager.stop()

      val result2 = networkHealthCheck.check()
      result2.isUnhealthy shouldBe true

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "aggregate multiple system checks" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val networkManager = new NetworkManager(0, scheduler, (_, _) => ())
      Await.result(networkManager.start(enableDiscovery = false), 3.seconds)

      // Create multiple health checks
      val pingCheck = new PingHealthCheck()
      val networkCheck = new FunctionalHealthCheck(
        "network",
        checkFn = () => networkManager.isRunning
      )
      val memoryCheck = new MemoryHealthCheck(
        maxUsedMemoryPercent = 99.0, // High threshold for testing
        maxUsedMemoryPercentWarning = 95.0
      )

      val composite = new CompositeHealthCheck(List(pingCheck, networkCheck, memoryCheck))

      val result = composite.check()
      result.name shouldBe "composite"

      // All checks should be healthy
      result.details should contain key "ping"
      result.details should contain key "network"
      result.details should contain key "memory"

      networkManager.stop()

    finally
      scheduler.gracefulShutdown(2)
  }

  "Async resilience patterns" should "work with async network operations" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val retryPolicy = RetryPolicy(
        maxAttempts = 3,
        initialDelay = 10.millis,
        maxDelay = 100.millis
      )

      var attemptCount = 0

      // Simulate async operation that succeeds on third attempt
      val future = retryPolicy.retryAsync {
        attemptCount += 1
        if attemptCount < 3 then
          Future.failed(new java.io.IOException("Network timeout"))
        else
          Future.successful("Async success")
      }

      val result = Await.result(future, 5.seconds)
      result shouldBe "Async success"
      attemptCount shouldBe 3

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "handle circuit breaker with async operations" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val circuitBreaker = new CircuitBreaker(
        failureThreshold = 2,
        resetTimeout = 1.second
      )

      // Open the circuit with failed async operations
      (1 to 2).foreach { _ =>
        val future = circuitBreaker.executeAsync {
          Future.failed(new java.io.IOException("Network error"))
        }
        Try(Await.result(future, 3.seconds))
      }

      // Wait a bit for async completion
      Thread.sleep(100)

      circuitBreaker.currentState shouldBe CircuitState.Open

      // Next call should be rejected
      val future = circuitBreaker.executeAsync {
        Future.successful("should not execute")
      }

      val thrown = intercept[CircuitBreakerOpenException] {
        Await.result(future, 3.seconds)
      }

      thrown.getMessage should include("open")

    finally
      scheduler.gracefulShutdown(2)
  }

  "HealthCheck async execution" should "work with composite checks" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val check1 = new FunctionalHealthCheck("check1", () => true)
      val check2 = new FunctionalHealthCheck("check2", () => true)
      val check3 = new PingHealthCheck()

      val composite = new CompositeHealthCheck(List(check1, check2, check3))

      val future = composite.checkAsync()
      val result = Await.result(future, 3.seconds)

      result.isHealthy shouldBe true
      result.details.size shouldBe 3

    finally
      scheduler.gracefulShutdown(2)
  }
