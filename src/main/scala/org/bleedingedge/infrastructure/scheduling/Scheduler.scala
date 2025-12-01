package org.bleedingedge.infrastructure.scheduling

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Modern scheduler for executing asynchronous tasks with proper resource management.
 *
 * This replaces the old ThreadPool object with a class-based design that supports:
 * - Configurable thread pool size
 * - Proper shutdown with graceful termination
 * - ExecutionContext for Scala Futures
 * - Thread safety and resource cleanup
 *
 * @param poolSize Number of worker threads in the pool (default: 4)
 * @since 2.0.0
 */
class Scheduler(val poolSize: Int = 4) extends LazyLogging:

  require(poolSize > 0, s"Pool size must be positive, got: $poolSize")

  private val executor: ExecutorService = Executors.newFixedThreadPool(poolSize)

  /**
   * ExecutionContext for running Futures.
   * Available as a given for implicit use in Future operations.
   */
  given executionContext: ExecutionContext = ExecutionContext.fromExecutor(executor)

  logger.info(s"Scheduler initialized with $poolSize worker threads")

  /**
   * Executes a task asynchronously and returns a Future with the result.
   *
   * @param name Descriptive name for the task (used in logging)
   * @param task The task to execute
   * @tparam T Return type of the task
   * @return Future containing the task result
   */
  def execute[T](name: String)(task: => T): Future[T] =
    logger.debug(s"Submitting task: $name")
    Future {
      try
        logger.debug(s"Executing task: $name")
        val result = task
        logger.debug(s"Task completed successfully: $name")
        result
      catch
        case e: Exception =>
          logger.error(s"Task failed with exception: $name", e)
          throw e
    }

  /**
   * Checks if the scheduler has been shut down.
   */
  def isShutdown: Boolean = executor.isShutdown

  /**
   * Checks if all tasks have completed after shutdown.
   */
  def isTerminated: Boolean = executor.isTerminated

  /**
   * Initiates an orderly shutdown of the scheduler.
   *
   * Previously submitted tasks are executed, but no new tasks will be accepted.
   * This method does not wait for previously submitted tasks to complete.
   */
  def shutdown(): Unit =
    logger.info("Initiating scheduler shutdown")
    executor.shutdown()

  /**
   * Attempts to stop all actively executing tasks and halts processing of waiting tasks.
   *
   * @return List of tasks that were awaiting execution
   */
  def shutdownNow(): List[Runnable] =
    logger.warn("Forcing immediate scheduler shutdown")
    import scala.jdk.CollectionConverters.*
    executor.shutdownNow().asScala.toList

  /**
   * Blocks until all tasks have completed after a shutdown request,
   * or the timeout occurs, or the current thread is interrupted.
   *
   * @param timeout Maximum time to wait
   * @param unit Time unit for the timeout
   * @return true if all tasks completed, false if timeout elapsed
   */
  def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
    logger.debug(s"Awaiting termination for $timeout ${unit.toString.toLowerCase}")
    val result = executor.awaitTermination(timeout, unit)
    if result then
      logger.info("All tasks completed successfully")
    else
      logger.warn(s"Timeout elapsed before all tasks completed")
    result

  /**
   * Performs a graceful shutdown with a timeout.
   *
   * This is the recommended way to shut down the scheduler:
   * 1. Initiates shutdown
   * 2. Waits for tasks to complete (up to timeout)
   * 3. Forces shutdown if timeout expires
   *
   * @param timeoutSeconds Maximum seconds to wait for graceful shutdown (default: 10)
   */
  def gracefulShutdown(timeoutSeconds: Int = 10): Unit =
    logger.info(s"Starting graceful shutdown (timeout: ${timeoutSeconds}s)")

    // Initiate shutdown
    shutdown()

    // Wait for tasks to complete
    if !awaitTermination(timeoutSeconds, TimeUnit.SECONDS) then
      logger.warn("Graceful shutdown timeout expired, forcing shutdown")
      val pending = shutdownNow()
      logger.warn(s"${pending.size} tasks were cancelled")

      // Wait a bit more for forced shutdown
      if !awaitTermination(1, TimeUnit.SECONDS) then
        logger.error("Some tasks did not terminate after forced shutdown")

    logger.info("Scheduler shutdown complete")

object Scheduler:

  /**
   * Creates a Scheduler with the default pool size.
   */
  def apply(): Scheduler = new Scheduler()

  /**
   * Creates a Scheduler with a specified pool size.
   */
  def apply(poolSize: Int): Scheduler = new Scheduler(poolSize)

  /**
   * Creates a Scheduler with pool size from configuration.
   */
  def fromConfig(): Scheduler =
    import com.typesafe.config.ConfigFactory
    val config = ConfigFactory.load()
    val poolSize = config.getInt("bleedingedge.scheduler.thread-pool-size")
    new Scheduler(poolSize)
