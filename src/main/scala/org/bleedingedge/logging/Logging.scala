/*
 * Copyright (c) 2012, 2025 Miles Hampson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.bleedingedge.logging

import com.typesafe.scalalogging.LazyLogging

/**
 * Modern logging trait using industry-standard Logback/SLF4J.
 *
 * This replaces the custom LocalLogger implementation with thread-safe,
 * configurable logging through scala-logging and Logback.
 *
 * Benefits over the old LocalLogger:
 * - Thread-safe by design (no synchronization needed)
 * - Configurable via logback.xml
 * - Multiple appenders (console, file, rolling, etc.)
 * - Standard log levels (TRACE, DEBUG, INFO, WARN, ERROR)
 * - No deprecated APIs (sun.reflect.Reflection, finalize)
 * - Automatic resource management
 *
 * Usage:
 * {{{
 *   class MyClass extends Logging {
 *     logger.info("Starting operation")
 *     logger.debug(s"Processing item: $item")
 *     logger.error("Operation failed", exception)
 *   }
 * }}}
 *
 * @since 2.0.0
 */
trait Logging extends LazyLogging:

  /**
   * Records an exception with full stack trace.
   *
   * Equivalent to the old recordException method.
   */
  def recordException(exception: Exception): Unit =
    logger.error("Exception occurred", exception)

  /**
   * Records an error with problem description and resolution.
   *
   * Equivalent to the old recordError method.
   */
  def recordError(problem: String, resolution: String): Unit =
    logger.error(s"$problem -> $resolution")

  /**
   * Records an informational event.
   *
   * Equivalent to the old recordEvent method.
   */
  def recordEvent(eventString: String): Unit =
    logger.info(eventString)

  /**
   * Records a debug message.
   *
   * Equivalent to the old recordDebug method.
   * Debug messages are only logged if DEBUG level is enabled in logback.xml.
   */
  def recordDebug(eventString: String): Unit =
    logger.debug(eventString)
