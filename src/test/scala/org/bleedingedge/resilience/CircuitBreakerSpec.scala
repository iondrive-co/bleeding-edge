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

package org.bleedingedge.infrastructure.resilience

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * Tests for circuit breaker pattern.
 */
class CircuitBreakerSpec extends AnyFlatSpec with Matchers:

  "CircuitBreaker" should "validate parameters" in {
    // Valid breaker
    noException should be thrownBy {
      new CircuitBreaker(failureThreshold = 5, resetTimeout = 30.seconds)
    }

    // Invalid failure threshold
    an[IllegalArgumentException] should be thrownBy {
      new CircuitBreaker(failureThreshold = 0)
    }

    // Invalid reset timeout
    an[IllegalArgumentException] should be thrownBy {
      new CircuitBreaker(resetTimeout = Duration.Zero)
    }

    // Invalid half-open max calls
    an[IllegalArgumentException] should be thrownBy {
      new CircuitBreaker(halfOpenMaxCalls = 0)
    }

    // Invalid sliding window size
    an[IllegalArgumentException] should be thrownBy {
      new CircuitBreaker(failureThreshold = 10, slidingWindowSize = 5)
    }
  }

  it should "start in Closed state" in {
    val breaker = new CircuitBreaker()
    breaker.currentState shouldBe CircuitState.Closed
    breaker.failures shouldBe 0
  }

  it should "allow calls in Closed state" in {
    val breaker = new CircuitBreaker()
    var callCount = 0

    val result = breaker.execute {
      callCount += 1
      "success"
    }

    result shouldBe Success("success")
    callCount shouldBe 1
  }

  it should "transition to Open after threshold failures" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 3,
      resetTimeout = 1.second
    )

    // Cause 3 failures
    (1 to 3).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    breaker.currentState shouldBe CircuitState.Open
    breaker.failures shouldBe 3
  }

  it should "reject calls when Open" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 1.second
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    // Try to call again
    val result = breaker.execute {
      "should not execute"
    }

    result.isFailure shouldBe true
    result.failed.get shouldBe a[CircuitBreakerOpenException]
  }

  it should "transition to HalfOpen after reset timeout" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 100.millis
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    breaker.currentState shouldBe CircuitState.Open

    // Wait for reset timeout
    Thread.sleep(150)

    // Next call should transition to HalfOpen
    var called = false
    breaker.execute {
      called = true
      "success"
    }

    called shouldBe true
    breaker.currentState shouldBe CircuitState.HalfOpen
  }

  it should "transition from HalfOpen to Closed after successful calls" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 100.millis,
      halfOpenMaxCalls = 3
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    // Wait for reset timeout
    Thread.sleep(150)

    // Make successful calls in half-open state
    (1 to 3).foreach { _ =>
      val result = breaker.execute("success")
      result shouldBe Success("success")
    }

    breaker.currentState shouldBe CircuitState.Closed
    breaker.failures shouldBe 0
  }

  it should "transition from HalfOpen back to Open on failure" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 100.millis,
      halfOpenMaxCalls = 3
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    // Wait for reset timeout
    Thread.sleep(150)

    // Make one successful call
    breaker.execute("success")
    breaker.currentState shouldBe CircuitState.HalfOpen

    // Fail on next call
    breaker.execute {
      throw new RuntimeException("Failure again")
    }

    breaker.currentState shouldBe CircuitState.Open
  }

  // Test removed - behavior covered by "transition from HalfOpen to Closed" test

  it should "reset failure count on success in Closed state" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 3,
      resetTimeout = 1.second
    )

    // Cause some failures (but not enough to open)
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    breaker.failures shouldBe 2

    // Successful call should reset count
    breaker.execute("success")

    breaker.failures shouldBe 0
    breaker.currentState shouldBe CircuitState.Closed
  }

  it should "handle async operations" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 3,
      resetTimeout = 1.second
    )

    var callCount = 0

    val future = breaker.executeAsync {
      callCount += 1
      Future.successful("success")
    }

    val result = Await.result(future, 3.seconds)
    result shouldBe "success"
    callCount shouldBe 1
  }

  it should "open on async failures" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 1.second
    )

    // Cause failures
    (1 to 2).foreach { _ =>
      val future = breaker.executeAsync {
        Future.failed(new RuntimeException("Async failure"))
      }

      Try {
        Await.result(future, 3.seconds)
      }
    }

    // Wait a bit for async completion
    Thread.sleep(100)

    breaker.currentState shouldBe CircuitState.Open
  }

  it should "reject async calls when Open" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 1.second
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    val future = breaker.executeAsync {
      Future.successful("should not execute")
    }

    val thrown = intercept[CircuitBreakerOpenException] {
      Await.result(future, 3.seconds)
    }

    thrown.getMessage should include("open")
  }

  it should "reset to Closed state" in {
    val breaker = new CircuitBreaker(
      failureThreshold = 2,
      resetTimeout = 1.second
    )

    // Open the circuit
    (1 to 2).foreach { _ =>
      breaker.execute {
        throw new RuntimeException("Failure")
      }
    }

    breaker.currentState shouldBe CircuitState.Open

    // Reset
    breaker.reset()

    breaker.currentState shouldBe CircuitState.Closed
    breaker.failures shouldBe 0
  }

  "CircuitBreaker.networkDefault" should "create with reasonable defaults" in {
    val breaker = CircuitBreaker.networkDefault()

    breaker.failureThreshold shouldBe 5
    breaker.resetTimeout shouldBe 30.seconds
    breaker.halfOpenMaxCalls shouldBe 3
  }

  "CircuitBreaker.aggressive" should "open quickly" in {
    val breaker = CircuitBreaker.aggressive()
    breaker.failureThreshold shouldBe 3
  }

  "CircuitBreaker.conservative" should "be more tolerant" in {
    val breaker = CircuitBreaker.conservative()
    breaker.failureThreshold shouldBe 10
  }
