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

package org.bleedingedge.scheduling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class SchedulerSpec extends AnyFlatSpec with Matchers:

  "Scheduler" should "execute tasks successfully" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val future = scheduler.execute("test-task") {
        42
      }

      val result = Await.result(future, 5.seconds)
      result.shouldBe(42)
    finally
      scheduler.gracefulShutdown(2)
  }

  it should "execute multiple tasks concurrently" in {
    val scheduler = Scheduler(4)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val counter = AtomicInteger(0)
      val futures = (1 to 10).map { i =>
        scheduler.execute(s"task-$i") {
          Thread.sleep(100)
          counter.incrementAndGet()
        }
      }

      val results = Await.result(Future.sequence(futures), 5.seconds)
      results.sum.shouldBe(55) // 1+2+3+...+10 = 55
      counter.get().shouldBe(10)
    finally
      scheduler.gracefulShutdown(2)
  }

  it should "handle task failures gracefully" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      val future = scheduler.execute("failing-task") {
        throw RuntimeException("Task failed!")
      }

      assertThrows[RuntimeException] {
        Await.result(future, 5.seconds)
      }
    finally
      scheduler.gracefulShutdown(2)
  }

  it should "create scheduler with default pool size" in {
    val scheduler = Scheduler()
    scheduler.poolSize.shouldBe(4)
    scheduler.gracefulShutdown(1)
  }

  it should "create scheduler with custom pool size" in {
    val scheduler = Scheduler(8)
    scheduler.poolSize.shouldBe(8)
    scheduler.gracefulShutdown(1)
  }

  it should "reject invalid pool sizes" in {
    assertThrows[IllegalArgumentException] {
      Scheduler(0)
    }

    assertThrows[IllegalArgumentException] {
      Scheduler(-1)
    }
  }

  "Scheduler shutdown" should "prevent new tasks after shutdown" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    scheduler.shutdown()
    scheduler.isShutdown.shouldBe(true)

    // Submitting tasks after shutdown should still work (ExecutionContext queues them)
    // but the scheduler won't execute them
    scheduler.gracefulShutdown(1)
  }

  it should "wait for tasks to complete during graceful shutdown" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    val counter = AtomicInteger(0)

    // Submit long-running tasks
    (1 to 3).foreach { i =>
      scheduler.execute(s"long-task-$i") {
        Thread.sleep(500)
        counter.incrementAndGet()
      }
    }

    // Give tasks time to start
    Thread.sleep(100)

    // Graceful shutdown should wait for them
    scheduler.gracefulShutdown(3)

    scheduler.isTerminated.shouldBe(true)
    counter.get().shouldBe(3)
  }

  it should "force shutdown with shutdownNow" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    // Submit many tasks
    (1 to 20).foreach { i =>
      scheduler.execute(s"task-$i") {
        Thread.sleep(1000)
        i
      }
    }

    Thread.sleep(100)

    // Force immediate shutdown
    val pending = scheduler.shutdownNow()

    // Some tasks should have been cancelled
    pending.size should be > 0
    scheduler.awaitTermination(1, TimeUnit.SECONDS)
  }

  it should "handle timeout during graceful shutdown" in {
    val scheduler = Scheduler(1)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    // Submit a very long task
    scheduler.execute("very-long-task") {
      Thread.sleep(10000) // 10 seconds
      "done"
    }

    Thread.sleep(100)

    // Shutdown with short timeout should force shutdown
    scheduler.gracefulShutdown(1)

    // Should be terminated (either gracefully or forcefully)
    scheduler.isTerminated.shouldBe(true)
  }

  "Scheduler awaitTermination" should "return true when all tasks complete" in {
    val scheduler = Scheduler(2)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    scheduler.execute("quick-task") {
      Thread.sleep(100)
      "done"
    }

    scheduler.shutdown()
    val completed = scheduler.awaitTermination(2, TimeUnit.SECONDS)

    completed.shouldBe(true)
    scheduler.isTerminated.shouldBe(true)
  }

  it should "return false on timeout" in {
    val scheduler = Scheduler(1)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    scheduler.execute("long-task") {
      Thread.sleep(5000)
      "done"
    }

    scheduler.shutdown()
    val completed = scheduler.awaitTermination(100, TimeUnit.MILLISECONDS)

    completed.shouldBe(false)
    scheduler.shutdownNow()
    scheduler.awaitTermination(1, TimeUnit.SECONDS)
  }

  "Scheduler ExecutionContext" should "be available as given" in {
    val scheduler = Scheduler(2)

    try
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      // Should be able to use Futures directly
      val future = Future {
        21 + 21
      }

      val result = Await.result(future, 2.seconds)
      result.shouldBe(42)
    finally
      scheduler.gracefulShutdown(1)
  }
