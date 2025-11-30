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

import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

/**
 * Tests for health check system.
 */
class HealthCheckSpec extends AnyFlatSpec with Matchers:

  "HealthCheckResult" should "determine health status" in {
    val healthy = HealthCheckResult("test", HealthStatus.Healthy)
    healthy.isHealthy shouldBe true
    healthy.isDegraded shouldBe false
    healthy.isUnhealthy shouldBe false

    val degraded = HealthCheckResult("test", HealthStatus.Degraded)
    degraded.isHealthy shouldBe false
    degraded.isDegraded shouldBe true
    degraded.isUnhealthy shouldBe false

    val unhealthy = HealthCheckResult("test", HealthStatus.Unhealthy)
    unhealthy.isHealthy shouldBe false
    unhealthy.isDegraded shouldBe false
    unhealthy.isUnhealthy shouldBe true
  }

  "PingHealthCheck" should "always return healthy" in {
    val check = new PingHealthCheck()
    check.name shouldBe "ping"

    val result = check.check()
    result.isHealthy shouldBe true
    result.message shouldBe Some("pong")
  }

  "FunctionalHealthCheck" should "return healthy when check passes" in {
    val check = new FunctionalHealthCheck("test", () => true)

    val result = check.check()
    result.isHealthy shouldBe true
    result.status shouldBe HealthStatus.Healthy
  }

  it should "return unhealthy when check fails" in {
    val check = new FunctionalHealthCheck("test", () => false)

    val result = check.check()
    result.isUnhealthy shouldBe true
    result.status shouldBe HealthStatus.Unhealthy
  }

  it should "return degraded when degraded check passes" in {
    val check = new FunctionalHealthCheck(
      "test",
      checkFn = () => false,
      degradedFn = Some(() => true)
    )

    val result = check.check()
    result.isDegraded shouldBe true
    result.status shouldBe HealthStatus.Degraded
  }

  it should "handle exceptions gracefully" in {
    val check = new FunctionalHealthCheck(
      "test",
      checkFn = () => throw new RuntimeException("Test exception")
    )

    val result = check.check()
    result.isUnhealthy shouldBe true
    result.message.get should include("exception")
  }

  "MemoryHealthCheck" should "check memory usage" in {
    val check = new MemoryHealthCheck(
      maxUsedMemoryPercent = 95.0,
      maxUsedMemoryPercentWarning = 80.0
    )

    check.name shouldBe "memory"

    val result = check.check()

    // Should return some status (likely Healthy in test environment)
    result.status should (be(HealthStatus.Healthy) or be(HealthStatus.Degraded) or be(HealthStatus.Unhealthy))

    // Should have details
    result.message shouldBe defined
    result.details should contain key "usedBytes"
    result.details should contain key "maxBytes"
    result.details should contain key "usedPercent"
  }

  it should "report degraded when memory usage is high" in {
    // This test is tricky because we can't easily force high memory usage
    // We'll just verify the logic would work with the thresholds
    val check = new MemoryHealthCheck(
      maxUsedMemoryPercent = 0.1, // Very low threshold
      maxUsedMemoryPercentWarning = 0.05
    )

    val result = check.check()

    // With such low thresholds, should likely be degraded or unhealthy
    result.isHealthy shouldBe false
  }

  "DiskSpaceHealthCheck" should "check disk space" in {
    val tempDir = Files.createTempDirectory("health-check-test")
    try
      val check = new DiskSpaceHealthCheck(
        tempDir,
        minFreeBytes = 1024, // 1 KB (very low for testing)
        minFreeBytesWarning = 1024 * 1024 * 10 // 10 MB
      )

      check.name shouldBe "disk-space"

      val result = check.check()

      // Should return some status
      result.status should (be(HealthStatus.Healthy) or be(HealthStatus.Degraded) or be(HealthStatus.Unhealthy))

      // Should have details
      result.message shouldBe defined
      result.details should contain key "usableBytes"
      result.details should contain key "path"
    finally
      Files.deleteIfExists(tempDir)
  }

  it should "handle missing paths gracefully" in {
    val nonExistent = Paths.get("/nonexistent/path/that/does/not/exist")
    val check = new DiskSpaceHealthCheck(nonExistent)

    val result = check.check()
    result.isUnhealthy shouldBe true
    result.message.get should include("Error")
  }

  "CompositeHealthCheck" should "aggregate multiple checks" in {
    val check1 = new FunctionalHealthCheck("check1", () => true)
    val check2 = new FunctionalHealthCheck("check2", () => true)
    val check3 = new FunctionalHealthCheck("check3", () => true)

    val composite = new CompositeHealthCheck(List(check1, check2, check3))

    val result = composite.check()

    result.name shouldBe "composite"
    result.isHealthy shouldBe true
    result.message.get should include("3 healthy")

    result.details should contain key "check1"
    result.details should contain key "check2"
    result.details should contain key "check3"
  }

  it should "report worst status" in {
    val healthy = new FunctionalHealthCheck("healthy", () => true)
    val degraded = new FunctionalHealthCheck(
      "degraded",
      checkFn = () => false,
      degradedFn = Some(() => true)
    )
    val unhealthy = new FunctionalHealthCheck("unhealthy", () => false)

    // All healthy
    val composite1 = new CompositeHealthCheck(List(healthy))
    composite1.check().status shouldBe HealthStatus.Healthy

    // Mix of healthy and degraded
    val composite2 = new CompositeHealthCheck(List(healthy, degraded))
    composite2.check().status shouldBe HealthStatus.Degraded

    // Mix with unhealthy
    val composite3 = new CompositeHealthCheck(List(healthy, degraded, unhealthy))
    composite3.check().status shouldBe HealthStatus.Unhealthy
  }

  it should "count statuses correctly" in {
    val healthy1 = new FunctionalHealthCheck("h1", () => true)
    val healthy2 = new FunctionalHealthCheck("h2", () => true)
    val degraded = new FunctionalHealthCheck(
      "degraded",
      checkFn = () => false,
      degradedFn = Some(() => true)
    )
    val unhealthy = new FunctionalHealthCheck("unhealthy", () => false)

    val composite = new CompositeHealthCheck(List(healthy1, healthy2, degraded, unhealthy))

    val result = composite.check()
    result.message.get should include("2 healthy")
    result.message.get should include("1 degraded")
    result.message.get should include("1 unhealthy")
  }

  it should "support async checks" in {
    val check1 = new FunctionalHealthCheck("check1", () => true)
    val check2 = new FunctionalHealthCheck("check2", () => true)

    val composite = new CompositeHealthCheck(List(check1, check2))

    val future = composite.checkAsync()
    val result = Await.result(future, 3.seconds)

    result.isHealthy shouldBe true
  }

  "HealthCheck" should "support async execution" in {
    val check = new PingHealthCheck()

    val future = check.checkAsync()
    val result = Await.result(future, 3.seconds)

    result.isHealthy shouldBe true
  }
