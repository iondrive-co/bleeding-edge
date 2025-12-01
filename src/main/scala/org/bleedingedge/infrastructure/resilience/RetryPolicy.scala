package org.bleedingedge.infrastructure.resilience

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Retry policy with exponential backoff.
 *
 * Provides configurable retry logic with exponential backoff and jitter
 * to prevent thundering herd problems.
 *
 * @param maxAttempts Maximum number of retry attempts
 * @param initialDelay Initial delay between retries
 * @param maxDelay Maximum delay between retries
 * @param backoffMultiplier Multiplier for exponential backoff (default: 2.0)
 * @param jitterFactor Random jitter factor (0.0 to 1.0, default: 0.1)
 * @since 3.0.0
 */
case class RetryPolicy(
  maxAttempts: Int = 3,
  initialDelay: FiniteDuration = 100.millis,
  maxDelay: FiniteDuration = 10.seconds,
  backoffMultiplier: Double = 2.0,
  jitterFactor: Double = 0.1
) extends LazyLogging:

  require(maxAttempts > 0, "maxAttempts must be positive")
  require(initialDelay.toMillis > 0, "initialDelay must be positive")
  require(maxDelay >= initialDelay, "maxDelay must be >= initialDelay")
  require(backoffMultiplier >= 1.0, "backoffMultiplier must be >= 1.0")
  require(jitterFactor >= 0.0 && jitterFactor <= 1.0, "jitterFactor must be between 0.0 and 1.0")

  /**
   * Calculates the delay for a given attempt number.
   *
   * Uses exponential backoff with jitter to spread out retries.
   *
   * @param attempt Attempt number (0-based)
   * @return Delay duration
   */
  def delayFor(attempt: Int): FiniteDuration =
    if attempt <= 0 then
      Duration.Zero
    else
      // Exponential backoff: initialDelay * (backoffMultiplier ^ (attempt - 1))
      val baseDelay = initialDelay.toMillis * Math.pow(backoffMultiplier, attempt - 1)

      // Cap at maxDelay
      val cappedDelay = Math.min(baseDelay, maxDelay.toMillis.toDouble)

      // Add jitter: random value between (1 - jitter) and (1 + jitter)
      val jitter = 1.0 + (scala.util.Random.nextDouble() * 2 - 1) * jitterFactor
      val finalDelay = (cappedDelay * jitter).toLong

      finalDelay.millis

  /**
   * Retries a synchronous operation.
   *
   * @param operation Operation to retry
   * @param shouldRetry Predicate to determine if exception should trigger retry
   * @tparam T Result type
   * @return Result or final exception
   */
  def retry[T](
    operation: => T,
    shouldRetry: Throwable => Boolean = _ => true
  ): Try[T] =
    def attempt(attemptNum: Int, lastError: Option[Throwable]): Try[T] =
      if attemptNum >= maxAttempts then
        lastError match
          case Some(e) =>
            logger.error(s"All $maxAttempts retry attempts exhausted", e)
            Failure(e)
          case None =>
            Failure(new RuntimeException("Retry failed with no error"))
      else
        Try(operation) match
          case Success(result) =>
            if attemptNum > 0 then
              logger.info(s"Operation succeeded on attempt ${attemptNum + 1}")
            Success(result)

          case Failure(e) if shouldRetry(e) =>
            val delay = delayFor(attemptNum)
            logger.warn(s"Attempt ${attemptNum + 1}/$maxAttempts failed, retrying after $delay", e)
            Thread.sleep(delay.toMillis)
            attempt(attemptNum + 1, Some(e))

          case Failure(e) =>
            logger.error(s"Operation failed with non-retryable error", e)
            Failure(e)

    attempt(0, None)

  /**
   * Retries an asynchronous operation.
   *
   * @param operation Async operation to retry
   * @param shouldRetry Predicate to determine if exception should trigger retry
   * @param ec Execution context
   * @tparam T Result type
   * @return Future of result
   */
  def retryAsync[T](
    operation: => Future[T],
    shouldRetry: Throwable => Boolean = _ => true
  )(using ec: ExecutionContext): Future[T] =
    def attempt(attemptNum: Int, lastError: Option[Throwable]): Future[T] =
      if attemptNum >= maxAttempts then
        lastError match
          case Some(e) =>
            logger.error(s"All $maxAttempts async retry attempts exhausted", e)
            Future.failed(e)
          case None =>
            Future.failed(new RuntimeException("Async retry failed with no error"))
      else
        operation.recoverWith {
          case e if shouldRetry(e) =>
            val delay = delayFor(attemptNum)
            logger.warn(s"Async attempt ${attemptNum + 1}/$maxAttempts failed, retrying after $delay", e)

            // Delay and retry
            val promise = scala.concurrent.Promise[T]()
            val timer = new java.util.Timer(true)
            timer.schedule(
              new java.util.TimerTask {
                def run(): Unit =
                  promise.completeWith(attempt(attemptNum + 1, Some(e)))
              },
              delay.toMillis
            )
            promise.future

          case e =>
            logger.error(s"Async operation failed with non-retryable error", e)
            Future.failed(e)
        }.andThen {
          case Success(_) if attemptNum > 0 =>
            logger.info(s"Async operation succeeded on attempt ${attemptNum + 1}")
        }

    attempt(0, None)

object RetryPolicy:
  /**
   * Default retry policy for network operations.
   */
  val networkDefault: RetryPolicy = RetryPolicy(
    maxAttempts = 3,
    initialDelay = 100.millis,
    maxDelay = 5.seconds,
    backoffMultiplier = 2.0,
    jitterFactor = 0.1
  )

  /**
   * Aggressive retry policy for critical operations.
   */
  val aggressive: RetryPolicy = RetryPolicy(
    maxAttempts = 5,
    initialDelay = 50.millis,
    maxDelay = 10.seconds,
    backoffMultiplier = 2.0,
    jitterFactor = 0.15
  )

  /**
   * Conservative retry policy for non-critical operations.
   */
  val conservative: RetryPolicy = RetryPolicy(
    maxAttempts = 2,
    initialDelay = 200.millis,
    maxDelay = 2.seconds,
    backoffMultiplier = 2.0,
    jitterFactor = 0.1
  )

  /**
   * No retry policy - fails immediately.
   */
  val noRetry: RetryPolicy = RetryPolicy(
    maxAttempts = 1,
    initialDelay = 1.milli, // Minimal delay to satisfy validation
    maxDelay = 1.milli
  )
