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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/**
 * Tests for retry policy with exponential backoff.
 */
class RetryPolicySpec extends AnyFlatSpec with Matchers:

  "RetryPolicy" should "validate parameters" in {
    // Valid policy
    noException should be thrownBy {
      RetryPolicy(maxAttempts = 3, initialDelay = 100.millis, maxDelay = 1.second)
    }

    // Invalid maxAttempts
    an[IllegalArgumentException] should be thrownBy {
      RetryPolicy(maxAttempts = 0)
    }

    // Invalid initialDelay
    an[IllegalArgumentException] should be thrownBy {
      RetryPolicy(initialDelay = Duration.Zero)
    }

    // Invalid maxDelay
    an[IllegalArgumentException] should be thrownBy {
      RetryPolicy(initialDelay = 2.seconds, maxDelay = 1.second)
    }

    // Invalid backoff multiplier
    an[IllegalArgumentException] should be thrownBy {
      RetryPolicy(backoffMultiplier = 0.5)
    }

    // Invalid jitter factor
    an[IllegalArgumentException] should be thrownBy {
      RetryPolicy(jitterFactor = 1.5)
    }
  }

  it should "calculate exponential backoff delays" in {
    val policy = RetryPolicy(
      maxAttempts = 5,
      initialDelay = 100.millis,
      maxDelay = 10.seconds,
      backoffMultiplier = 2.0,
      jitterFactor = 0.0 // No jitter for predictable testing
    )

    // Attempt 0: no delay
    policy.delayFor(0) shouldBe Duration.Zero

    // Attempt 1: initial delay
    policy.delayFor(1).toMillis shouldBe 100L

    // Attempt 2: initialDelay * 2^1
    policy.delayFor(2).toMillis shouldBe 200L

    // Attempt 3: initialDelay * 2^2
    policy.delayFor(3).toMillis shouldBe 400L

    // Attempt 4: initialDelay * 2^3
    policy.delayFor(4).toMillis shouldBe 800L
  }

  it should "cap delays at maxDelay" in {
    val policy = RetryPolicy(
      maxAttempts = 10,
      initialDelay = 100.millis,
      maxDelay = 500.millis,
      backoffMultiplier = 2.0,
      jitterFactor = 0.0
    )

    // These should be capped at 500ms
    policy.delayFor(5).toMillis should be <= 500L
    policy.delayFor(10).toMillis should be <= 500L
  }

  it should "add jitter to delays" in {
    val policy = RetryPolicy(
      maxAttempts = 3,
      initialDelay = 100.millis,
      maxDelay = 1.second,
      backoffMultiplier = 2.0,
      jitterFactor = 0.1
    )

    // With 10% jitter, delays should vary by ±10%
    val delays = (1 to 10).map(_ => policy.delayFor(1).toMillis)

    // Should not all be the same
    delays.toSet.size should be > 1

    // Should be within jitter range (90-110ms)
    delays.foreach { delay =>
      delay should be >= 90L
      delay should be <= 110L
    }
  }

  it should "succeed immediately on successful operation" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    val result = policy.retry {
      callCount += 1
      "success"
    }

    result shouldBe Success("success")
    callCount shouldBe 1
  }

  it should "retry on failure" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    val result = policy.retry {
      callCount += 1
      if callCount < 3 then
        throw new RuntimeException("Temporary failure")
      "success"
    }

    result shouldBe Success("success")
    callCount shouldBe 3
  }

  it should "fail after exhausting retries" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    val result = policy.retry {
      callCount += 1
      throw new RuntimeException("Persistent failure")
    }

    result.isFailure shouldBe true
    result.failed.get.getMessage shouldBe "Persistent failure"
    callCount shouldBe 3
  }

  it should "respect shouldRetry predicate" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    // Don't retry on IllegalArgumentException
    val result = policy.retry(
      {
        callCount += 1
        throw new IllegalArgumentException("Non-retryable")
      },
      shouldRetry = {
        case _: IllegalArgumentException => false
        case _ => true
      }
    )

    result.isFailure shouldBe true
    callCount shouldBe 1 // Should not retry
  }

  it should "retry async operations" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    val future = policy.retryAsync {
      callCount += 1
      if callCount < 3 then
        Future.failed(new RuntimeException("Temporary failure"))
      else
        Future.successful("success")
    }

    val result = Await.result(future, 5.seconds)
    result shouldBe "success"
    callCount shouldBe 3
  }

  it should "fail async operations after exhausting retries" in {
    val policy = RetryPolicy(maxAttempts = 3, initialDelay = 10.millis, maxDelay = 100.millis)
    var callCount = 0

    val future = policy.retryAsync {
      callCount += 1
      Future.failed(new RuntimeException("Persistent failure"))
    }

    val thrown = intercept[RuntimeException] {
      Await.result(future, 5.seconds)
    }

    thrown.getMessage shouldBe "Persistent failure"
    callCount shouldBe 3
  }

  "RetryPolicy.networkDefault" should "have reasonable defaults" in {
    val policy = RetryPolicy.networkDefault

    policy.maxAttempts shouldBe 3
    policy.initialDelay shouldBe 100.millis
    policy.maxDelay shouldBe 5.seconds
    policy.backoffMultiplier shouldBe 2.0
    policy.jitterFactor shouldBe 0.1
  }

  "RetryPolicy.aggressive" should "retry more times" in {
    val policy = RetryPolicy.aggressive
    policy.maxAttempts shouldBe 5
  }

  "RetryPolicy.conservative" should "retry fewer times" in {
    val policy = RetryPolicy.conservative
    policy.maxAttempts shouldBe 2
  }

  "RetryPolicy.noRetry" should "not retry" in {
    val policy = RetryPolicy.noRetry
    var callCount = 0

    val result = policy.retry {
      callCount += 1
      throw new RuntimeException("Failure")
    }

    result.isFailure shouldBe true
    callCount shouldBe 1 // No retries
  }
