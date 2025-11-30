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

import org.bleedingedge.config.BleedingEdgeConfig
import org.bleedingedge.network.NetworkManager
import org.bleedingedge.scheduling.Scheduler
import org.bleedingedge.sync.SyncManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

/**
 * Integration tests for configuration loading and component initialization.
 *
 * Tests that configuration can be loaded and applied to all system components.
 */
class ConfigurationIntegrationSpec extends AnyFlatSpec with Matchers:

  "BleedingEdgeConfig" should "use default configuration" in {
    val config = BleedingEdgeConfig.default

    // Verify default values
    config.port shouldBe 8888
    config.discoveryEnabled shouldBe true
    config.schedulerThreads should be > 0
    config.multicastGroup should not be empty
  }

  it should "create components from configuration" in {
    val config = BleedingEdgeConfig.default.copy(port = 0) // Use random port
    val scheduler = Scheduler(config.schedulerThreads)
    given scala.concurrent.ExecutionContext = scheduler.executionContext

    try
      // Create temporary sync directory
      val tempDir = Files.createTempDirectory("config-test-")

      try
        // Create sync manager with configured directory
        val syncManager = SyncManager(tempDir, scheduler)

        // Create network manager with random port
        val networkManager = new NetworkManager(
          0, // Use random port to avoid conflicts
          scheduler,
          (_, _) => () // Simple message handler
        )

        // Start components
        Await.result(networkManager.start(enableDiscovery = false), 5.seconds)
        Await.result(syncManager.start(), 5.seconds)

        // Verify components are running
        networkManager.isRunning shouldBe true
        syncManager.isRunning shouldBe true

        // Cleanup
        syncManager.stop()
        networkManager.stop()

      finally
        // Delete temp directory
        Files.walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete)

    finally
      scheduler.gracefulShutdown(2)
  }

  it should "load configuration from file" in {
    val tempDir = Files.createTempDirectory("test-config-")

    try
      // Create .bleedingedge directory
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)

      // Write test configuration
      val configContent = """
        |# Test configuration
        |port = 9999
        |
        |discovery {
        |  enabled = false
        |  multicast-group = "230.0.0.1"
        |  multicast-port = 4446
        |}
        |
        |scheduler {
        |  thread-pool-size = 8
        |}
        |
        |daemon {
        |  enabled = false
        |}
        |""".stripMargin

      val configFile = configDir.resolve("config")
      Files.writeString(configFile, configContent)

      // Load configuration from directory
      val config = BleedingEdgeConfig.load(tempDir)

      config.isRight shouldBe true
      val cfg = config.toOption.get

      // Verify loaded values
      cfg.port shouldBe 9999
      cfg.discoveryEnabled shouldBe false
      cfg.schedulerThreads shouldBe 8
      cfg.daemonMode shouldBe false

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "validate configuration values" in {
    val tempDir = Files.createTempDirectory("invalid-config-")

    try
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)

      // Write invalid configuration (negative pool size)
      val configContent = """
        |port = -999
        |
        |scheduler {
        |  thread-pool-size = 0
        |}
        |""".stripMargin

      val configFile = configDir.resolve("config")
      Files.writeString(configFile, configContent)

      // Load configuration should fail validation
      val config = BleedingEdgeConfig.load(tempDir)

      config.isLeft shouldBe true
      config.left.toOption.get should include("port")

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "handle missing configuration file gracefully" in {
    val tempDir = Files.createTempDirectory("no-config-")

    try
      // Load configuration from directory without config file (should use defaults)
      val config = BleedingEdgeConfig.load(tempDir)

      config.isRight shouldBe true
      val cfg = config.toOption.get

      // Should have default values
      cfg.port shouldBe BleedingEdgeConfig.default.port
      cfg.discoveryEnabled shouldBe BleedingEdgeConfig.default.discoveryEnabled

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "Configuration with NetworkManager" should "apply network settings" in {
    val tempDir = Files.createTempDirectory("network-config-")

    try
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)

      val configContent = """
        |port = 9100
        |
        |discovery {
        |  enabled = false
        |}
        |
        |scheduler {
        |  thread-pool-size = 2
        |}
        |""".stripMargin

      val configFile = configDir.resolve("config")
      Files.writeString(configFile, configContent)

      val loadResult = BleedingEdgeConfig.load(tempDir)
      loadResult.isRight shouldBe true

      val config = loadResult.toOption.get
      val scheduler = Scheduler(config.schedulerThreads)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        // Use random port to avoid conflicts, but verify config was loaded
        config.port shouldBe 9100
        config.discoveryEnabled shouldBe false

        val networkManager = new NetworkManager(
          0, // Use random port to avoid conflicts
          scheduler,
          (_, _) => () // Simple message handler
        )

        Await.result(networkManager.start(enableDiscovery = config.discoveryEnabled), 5.seconds)

        // Verify settings
        networkManager.isRunning shouldBe true
        networkManager.port should be >= 0

        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "Configuration with SyncManager" should "apply sync directory settings" in {
    val tempSyncDir = Files.createTempDirectory("sync-test-")

    try
      val config = BleedingEdgeConfig.default
      val scheduler = Scheduler(config.schedulerThreads)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        val syncManager = SyncManager(tempSyncDir, scheduler)

        Await.result(syncManager.start(), 5.seconds)

        // Verify sync manager is monitoring the configured directory
        syncManager.isRunning shouldBe true

        syncManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Files.walk(tempSyncDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "Full system integration" should "initialize all components from configuration" in {
    val tempSyncDir = Files.createTempDirectory("full-system-sync-")

    try
      val config = BleedingEdgeConfig.default.copy(
        port = 0, // Random port
        discoveryEnabled = false
      )

      // Initialize all components
      val scheduler = Scheduler(config.schedulerThreads)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      try
        val syncManager = SyncManager(tempSyncDir, scheduler)
        val networkManager = new NetworkManager(
          config.port,
          scheduler,
          (_, _) => () // Simple message handler
        )

        // Start all components
        Await.result(networkManager.start(enableDiscovery = config.discoveryEnabled), 5.seconds)
        Await.result(syncManager.start(), 5.seconds)

        // Verify entire system is running
        scheduler.executionContext should not be null
        syncManager.isRunning shouldBe true
        networkManager.isRunning shouldBe true

        // Cleanup
        syncManager.stop()
        networkManager.stop()

      finally
        scheduler.gracefulShutdown(2)

    finally
      Files.walk(tempSyncDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }
