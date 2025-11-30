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

package org.bleedingedge.config

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import com.typesafe.scalalogging.LazyLogging

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Configuration for BleedingEdge application.
 *
 * Loads configuration from HOCON files with fallback to defaults.
 *
 * @param port Network port for peer connections
 * @param discoveryEnabled Enable automatic peer discovery
 * @param multicastGroup Multicast group for discovery
 * @param multicastPort Multicast port for discovery
 * @param schedulerThreads Number of scheduler threads
 * @param daemonMode Run as background daemon
 * @since 3.0.0
 */
case class BleedingEdgeConfig(
  port: Int = 8888,
  discoveryEnabled: Boolean = true,
  multicastGroup: String = "230.0.0.1",
  multicastPort: Int = 4446,
  schedulerThreads: Int = 4,
  daemonMode: Boolean = false
):

  /**
   * Validates the configuration.
   */
  def validate(): Either[String, Unit] =
    if port < 1 || port > 65535 then
      Left(s"Invalid port: $port (must be 1-65535)")
    else if schedulerThreads < 1 then
      Left(s"Invalid scheduler threads: $schedulerThreads (must be >= 1)")
    else if multicastPort < 1 || multicastPort > 65535 then
      Left(s"Invalid multicast port: $multicastPort (must be 1-65535)")
    else
      Right(())

object BleedingEdgeConfig extends LazyLogging:

  /**
   * Default configuration.
   */
  val default: BleedingEdgeConfig = BleedingEdgeConfig()

  /**
   * Loads configuration from a directory.
   *
   * Looks for .bleedingedge/config file in HOCON format.
   * Falls back to defaults if file doesn't exist.
   *
   * @param directory Base directory containing .bleedingedge config
   * @return Configuration or error message
   */
  def load(directory: Path): Either[String, BleedingEdgeConfig] =
    val configDir = directory.resolve(".bleedingedge")
    val configFile = configDir.resolve("config")

    if !Files.exists(configFile) then
      logger.info(s"Config file not found, using defaults: $configFile")
      Right(default)
    else
      Try {
        logger.info(s"Loading configuration from: $configFile")

        // Load config file
        val fileConfig = ConfigFactory.parseFile(configFile.toFile)

        // Merge with defaults
        val config = fileConfig.withFallback(ConfigFactory.parseMap(Map(
          "port" -> default.port,
          "discovery.enabled" -> default.discoveryEnabled,
          "discovery.multicast-group" -> default.multicastGroup,
          "discovery.multicast-port" -> default.multicastPort,
          "scheduler.thread-pool-size" -> default.schedulerThreads,
          "daemon.enabled" -> default.daemonMode
        ).asJava))

        // Extract values
        BleedingEdgeConfig(
          port = config.getInt("port"),
          discoveryEnabled = config.getBoolean("discovery.enabled"),
          multicastGroup = config.getString("discovery.multicast-group"),
          multicastPort = config.getInt("discovery.multicast-port"),
          schedulerThreads = config.getInt("scheduler.thread-pool-size"),
          daemonMode = config.getBoolean("daemon.enabled")
        )
      } match
        case Success(cfg) =>
          cfg.validate() match
            case Right(_) =>
              logger.info(s"Configuration loaded successfully: port=${cfg.port}, discovery=${cfg.discoveryEnabled}")
              Right(cfg)
            case Left(error) =>
              logger.error(s"Configuration validation failed: $error")
              Left(error)

        case Failure(e) =>
          val error = s"Failed to load configuration: ${e.getMessage}"
          logger.error(error, e)
          Left(error)

  /**
   * Loads configuration with command-line overrides.
   *
   * @param directory Base directory
   * @param portOverride Optional port override from CLI
   * @param discoveryOverride Optional discovery override from CLI
   * @return Configuration or error message
   */
  def loadWithOverrides(
    directory: Path,
    portOverride: Option[Int] = None,
    discoveryOverride: Option[Boolean] = None,
    daemonOverride: Option[Boolean] = None
  ): Either[String, BleedingEdgeConfig] =
    load(directory).map { config =>
      config.copy(
        port = portOverride.getOrElse(config.port),
        discoveryEnabled = discoveryOverride.getOrElse(config.discoveryEnabled),
        daemonMode = daemonOverride.getOrElse(config.daemonMode)
      )
    }

  /**
   * Generates default configuration file content.
   */
  def defaultConfigContent: String =
    s"""# BleedingEdge Configuration
       |# HOCON format - see https://github.com/lightbend/config
       |
       |# Network port for peer connections
       |port = ${default.port}
       |
       |# Peer discovery settings
       |discovery {
       |  enabled = ${default.discoveryEnabled}
       |  multicast-group = "${default.multicastGroup}"
       |  multicast-port = ${default.multicastPort}
       |}
       |
       |# Scheduler configuration
       |scheduler {
       |  thread-pool-size = ${default.schedulerThreads}
       |}
       |
       |# Daemon mode
       |daemon {
       |  enabled = false
       |  pid-file = ".bleedingedge/bleedingedge.pid"
       |}
       |""".stripMargin
