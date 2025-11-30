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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Tests for BleedingEdge configuration management.
 */
class BleedingEdgeConfigSpec extends AnyFlatSpec with Matchers:

  "BleedingEdgeConfig" should "have sensible defaults" in {
    val config = BleedingEdgeConfig.default
    config.port shouldBe 8888
    config.discoveryEnabled shouldBe true
    config.multicastGroup shouldBe "230.0.0.1"
    config.multicastPort shouldBe 4446
    config.schedulerThreads shouldBe 4
    config.daemonMode shouldBe false
  }

  it should "validate valid configurations" in {
    val config = BleedingEdgeConfig(
      port = 9000,
      discoveryEnabled = false,
      multicastGroup = "224.0.0.1",
      multicastPort = 5000,
      schedulerThreads = 8,
      daemonMode = true
    )
    config.validate() shouldBe Right(())
  }

  it should "reject invalid port numbers" in {
    val config1 = BleedingEdgeConfig(port = 0)
    config1.validate().isLeft shouldBe true
    config1.validate().swap.getOrElse("") should include("Invalid port")

    val config2 = BleedingEdgeConfig(port = 65536)
    config2.validate().isLeft shouldBe true
    config2.validate().swap.getOrElse("") should include("Invalid port")

    val config3 = BleedingEdgeConfig(port = -100)
    config3.validate().isLeft shouldBe true
  }

  it should "reject invalid multicast port numbers" in {
    val config1 = BleedingEdgeConfig(multicastPort = 0)
    config1.validate().isLeft shouldBe true
    config1.validate().swap.getOrElse("") should include("Invalid multicast port")

    val config2 = BleedingEdgeConfig(multicastPort = 70000)
    config2.validate().isLeft shouldBe true
  }

  it should "reject invalid scheduler thread count" in {
    val config = BleedingEdgeConfig(schedulerThreads = 0)
    config.validate().isLeft shouldBe true
    config.validate().swap.getOrElse("") should include("Invalid scheduler threads")

    val config2 = BleedingEdgeConfig(schedulerThreads = -5)
    config2.validate().isLeft shouldBe true
  }

  "BleedingEdgeConfig.load" should "return defaults when no config file exists" in {
    val tempDir = Files.createTempDirectory("config-test-nofile")
    try
      val result = BleedingEdgeConfig.load(tempDir)
      result shouldBe Right(BleedingEdgeConfig.default)
    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "load valid HOCON configuration" in {
    val tempDir = Files.createTempDirectory("config-test-valid")
    try
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)
      val configFile = configDir.resolve("config")

      val configContent = """
        |port = 9999
        |discovery {
        |  enabled = false
        |  multicast-group = "224.0.0.1"
        |  multicast-port = 5555
        |}
        |scheduler {
        |  thread-pool-size = 16
        |}
        |daemon {
        |  enabled = true
        |}
        |""".stripMargin

      Files.writeString(configFile, configContent)

      val result = BleedingEdgeConfig.load(tempDir)
      result.isRight shouldBe true

      val config = result.getOrElse(BleedingEdgeConfig.default)
      config.port shouldBe 9999
      config.discoveryEnabled shouldBe false
      config.multicastGroup shouldBe "224.0.0.1"
      config.multicastPort shouldBe 5555
      config.schedulerThreads shouldBe 16
      config.daemonMode shouldBe true

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "merge with defaults for partial config" in {
    val tempDir = Files.createTempDirectory("config-test-partial")
    try
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)
      val configFile = configDir.resolve("config")

      // Only specify port, rest should use defaults
      val configContent = "port = 7777"
      Files.writeString(configFile, configContent)

      val result = BleedingEdgeConfig.load(tempDir)
      result.isRight shouldBe true

      val config = result.getOrElse(BleedingEdgeConfig.default)
      config.port shouldBe 7777
      config.discoveryEnabled shouldBe BleedingEdgeConfig.default.discoveryEnabled
      config.multicastGroup shouldBe BleedingEdgeConfig.default.multicastGroup
      config.schedulerThreads shouldBe BleedingEdgeConfig.default.schedulerThreads

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "reject invalid configuration values" in {
    val tempDir = Files.createTempDirectory("config-test-invalid")
    try
      val configDir = tempDir.resolve(".bleedingedge")
      Files.createDirectories(configDir)
      val configFile = configDir.resolve("config")

      // Invalid port number
      val configContent = "port = 99999"
      Files.writeString(configFile, configContent)

      val result = BleedingEdgeConfig.load(tempDir)
      result.isLeft shouldBe true
      result.swap.getOrElse("") should include("Invalid port")

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "BleedingEdgeConfig.loadWithOverrides" should "apply port override" in {
    val tempDir = Files.createTempDirectory("config-test-override")
    try
      val result = BleedingEdgeConfig.loadWithOverrides(
        tempDir,
        portOverride = Some(7000)
      )
      result.isRight shouldBe true
      result.getOrElse(BleedingEdgeConfig.default).port shouldBe 7000
    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "apply discovery override" in {
    val tempDir = Files.createTempDirectory("config-test-discovery")
    try
      val result = BleedingEdgeConfig.loadWithOverrides(
        tempDir,
        discoveryOverride = Some(false)
      )
      result.isRight shouldBe true
      result.getOrElse(BleedingEdgeConfig.default).discoveryEnabled shouldBe false
    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "apply daemon mode override" in {
    val tempDir = Files.createTempDirectory("config-test-daemon")
    try
      val result = BleedingEdgeConfig.loadWithOverrides(
        tempDir,
        daemonOverride = Some(true)
      )
      result.isRight shouldBe true
      result.getOrElse(BleedingEdgeConfig.default).daemonMode shouldBe true
    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "apply multiple overrides" in {
    val tempDir = Files.createTempDirectory("config-test-multiple")
    try
      val result = BleedingEdgeConfig.loadWithOverrides(
        tempDir,
        portOverride = Some(6000),
        discoveryOverride = Some(false),
        daemonOverride = Some(true)
      )
      result.isRight shouldBe true

      val config = result.getOrElse(BleedingEdgeConfig.default)
      config.port shouldBe 6000
      config.discoveryEnabled shouldBe false
      config.daemonMode shouldBe true
    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "BleedingEdgeConfig.defaultConfigContent" should "generate valid HOCON" in {
    val content = BleedingEdgeConfig.defaultConfigContent
    content should include("port = 8888")
    content should include("discovery")
    content should include("scheduler")
    content should include("daemon")
    content should include("# BleedingEdge Configuration")
  }
