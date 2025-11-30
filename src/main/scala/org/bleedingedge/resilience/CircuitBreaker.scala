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

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Circuit breaker states.
 */
enum CircuitState:
  case Closed   // Normal operation
  case Open     // Failing, rejecting calls
  case HalfOpen // Testing if service recovered

/**
 * Exception thrown when circuit breaker is open.
 */
class CircuitBreakerOpenException(message: String) extends RuntimeException(message)

/**
 * Circuit breaker for protecting against cascading failures.
 *
 * Implements the circuit breaker pattern to prevent repeated calls to
 * a failing service, giving it time to recover.
 *
 * States:
 * - Closed: Normal operation, calls pass through
 * - Open: Service is failing, calls are rejected immediately
 * - HalfOpen: Testing if service recovered, limited calls allowed
 *
 * @param failureThreshold Number of failures before opening circuit
 * @param resetTimeout Time to wait before attempting to close circuit
 * @param halfOpenMaxCalls Maximum calls to allow in half-open state
 * @param slidingWindowSize Size of sliding window for failure counting
 * @since 3.0.0
 */
class CircuitBreaker(
  val failureThreshold: Int = 5,
  val resetTimeout: FiniteDuration = 30.seconds,
  val halfOpenMaxCalls: Int = 3,
  val slidingWindowSize: Int = 10
) extends LazyLogging:

  require(failureThreshold > 0, "failureThreshold must be positive")
  require(resetTimeout.toMillis > 0, "resetTimeout must be positive")
  require(halfOpenMaxCalls > 0, "halfOpenMaxCalls must be positive")
  require(slidingWindowSize >= failureThreshold, "slidingWindowSize must be >= failureThreshold")

  private val state = new AtomicReference[CircuitState](CircuitState.Closed)
  private val failureCount = new AtomicInteger(0)
  private val successCount = new AtomicInteger(0)
  private val lastFailureTime = new AtomicLong(0)
  private val halfOpenCalls = new AtomicInteger(0)

  /**
   * Gets the current circuit state.
   */
  def currentState: CircuitState = state.get()

  /**
   * Gets the current failure count.
   */
  def failures: Int = failureCount.get()

  /**
   * Gets the current success count.
   */
  def successes: Int = successCount.get()

  /**
   * Resets the circuit breaker to closed state.
   */
  def reset(): Unit =
    state.set(CircuitState.Closed)
    failureCount.set(0)
    successCount.set(0)
    halfOpenCalls.set(0)
    lastFailureTime.set(0)
    logger.info("Circuit breaker reset to Closed state")

  /**
   * Checks if circuit should transition from Open to HalfOpen.
   */
  private def shouldAttemptReset(): Boolean =
    val lastFailure = lastFailureTime.get()
    val now = System.currentTimeMillis()
    (now - lastFailure) >= resetTimeout.toMillis

  /**
   * Records a successful call.
   */
  private def recordSuccess(): Unit =
    state.get() match
      case CircuitState.Closed =>
        successCount.incrementAndGet()
        // Reset failure count on success in closed state
        if failureCount.get() > 0 then
          failureCount.set(0)

      case CircuitState.HalfOpen =>
        val successes = successCount.incrementAndGet()
        logger.info(s"Success in HalfOpen state ($successes/${halfOpenMaxCalls})")

        // If we've had enough successful calls, close the circuit
        if successes >= halfOpenMaxCalls then
          state.set(CircuitState.Closed)
          failureCount.set(0)
          successCount.set(0)
          halfOpenCalls.set(0)
          logger.info("Circuit breaker transitioned to Closed after successful test calls")

      case CircuitState.Open =>
        // Shouldn't happen, but log it
        logger.warn("Unexpected success in Open state")

  /**
   * Records a failed call.
   */
  private def recordFailure(): Unit =
    lastFailureTime.set(System.currentTimeMillis())

    state.get() match
      case CircuitState.Closed =>
        val failures = failureCount.incrementAndGet()
        logger.warn(s"Failure recorded in Closed state ($failures/$failureThreshold)")

        if failures >= failureThreshold then
          state.set(CircuitState.Open)
          logger.error(s"Circuit breaker opened after $failures failures")

      case CircuitState.HalfOpen =>
        // Any failure in half-open immediately opens circuit
        state.set(CircuitState.Open)
        failureCount.set(failureThreshold) // Set to threshold to keep it open
        successCount.set(0)
        halfOpenCalls.set(0)
        logger.error("Circuit breaker returned to Open state after failure in HalfOpen")

      case CircuitState.Open =>
        failureCount.incrementAndGet()

  /**
   * Attempts to allow a call through the circuit breaker.
   *
   * @return true if call is allowed, false otherwise
   */
  private def allowCall(): Boolean =
    state.get() match
      case CircuitState.Closed =>
        true

      case CircuitState.Open =>
        // Check if we should transition to half-open
        if shouldAttemptReset() then
          if state.compareAndSet(CircuitState.Open, CircuitState.HalfOpen) then
            successCount.set(0)
            halfOpenCalls.set(0)
            logger.info("Circuit breaker transitioned to HalfOpen for testing")
            true
          else
            // Another thread changed the state
            allowCall()
        else
          false

      case CircuitState.HalfOpen =>
        // Allow limited calls in half-open state
        val calls = halfOpenCalls.incrementAndGet()
        if calls <= halfOpenMaxCalls then
          true
        else
          halfOpenCalls.decrementAndGet()
          false

  /**
   * Executes an operation through the circuit breaker.
   *
   * @param operation Operation to execute
   * @tparam T Result type
   * @return Result or exception
   */
  def execute[T](operation: => T): Try[T] =
    if !allowCall() then
      logger.debug("Circuit breaker is Open, rejecting call")
      Failure(new CircuitBreakerOpenException("Circuit breaker is open"))
    else
      Try(operation) match
        case Success(result) =>
          recordSuccess()
          Success(result)

        case Failure(e) =>
          recordFailure()
          Failure(e)

  /**
   * Executes an asynchronous operation through the circuit breaker.
   *
   * @param operation Async operation to execute
   * @param ec Execution context
   * @tparam T Result type
   * @return Future of result
   */
  def executeAsync[T](operation: => Future[T])(using ec: ExecutionContext): Future[T] =
    if !allowCall() then
      logger.debug("Circuit breaker is Open, rejecting async call")
      Future.failed(new CircuitBreakerOpenException("Circuit breaker is open"))
    else
      operation.andThen {
        case Success(_) => recordSuccess()
        case Failure(_) => recordFailure()
      }

object CircuitBreaker:
  /**
   * Default circuit breaker for network operations.
   */
  def networkDefault(): CircuitBreaker = new CircuitBreaker(
    failureThreshold = 5,
    resetTimeout = 30.seconds,
    halfOpenMaxCalls = 3,
    slidingWindowSize = 10
  )

  /**
   * Aggressive circuit breaker that opens quickly.
   */
  def aggressive(): CircuitBreaker = new CircuitBreaker(
    failureThreshold = 3,
    resetTimeout = 60.seconds,
    halfOpenMaxCalls = 2,
    slidingWindowSize = 5
  )

  /**
   * Conservative circuit breaker that's more tolerant of failures.
   */
  def conservative(): CircuitBreaker = new CircuitBreaker(
    failureThreshold = 10,
    resetTimeout = 15.seconds,
    halfOpenMaxCalls = 5,
    slidingWindowSize = 20
  )
