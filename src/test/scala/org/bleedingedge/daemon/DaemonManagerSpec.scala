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

package org.bleedingedge.daemon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/**
 * Tests for daemon process management.
 */
class DaemonManagerSpec extends AnyFlatSpec with Matchers:

  "DaemonManager.currentPid" should "return a valid PID" in {
    val pid = DaemonManager.currentPid
    pid should be > 0L
  }

  it should "return consistent PID across calls" in {
    val pid1 = DaemonManager.currentPid
    val pid2 = DaemonManager.currentPid
    pid1 shouldBe pid2
  }

  "DaemonManager.writePidFile" should "create a PID file with current PID" in {
    val tempDir = Files.createTempDirectory("daemon-test-write")
    try
      val pidFile = tempDir.resolve("test.pid")
      val result = DaemonManager.writePidFile(pidFile)

      result shouldBe Right(())
      Files.exists(pidFile) shouldBe true

      val writtenPid = Files.readString(pidFile).trim.toLong
      writtenPid shouldBe DaemonManager.currentPid

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "create parent directories if needed" in {
    val tempDir = Files.createTempDirectory("daemon-test-parent")
    try
      val pidFile = tempDir.resolve("nested/dir/test.pid")
      val result = DaemonManager.writePidFile(pidFile)

      result shouldBe Right(())
      Files.exists(pidFile) shouldBe true

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "DaemonManager.readPidFile" should "read PID from file" in {
    val tempDir = Files.createTempDirectory("daemon-test-read")
    try
      val pidFile = tempDir.resolve("test.pid")
      val testPid = 12345L
      Files.writeString(pidFile, testPid.toString)

      val result = DaemonManager.readPidFile(pidFile)
      result shouldBe Right(testPid)

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "return error for non-existent file" in {
    val tempDir = Files.createTempDirectory("daemon-test-nofile")
    try
      val pidFile = tempDir.resolve("nonexistent.pid")
      val result = DaemonManager.readPidFile(pidFile)

      result.isLeft shouldBe true
      result.swap.getOrElse("") should include("does not exist")

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "return error for invalid PID content" in {
    val tempDir = Files.createTempDirectory("daemon-test-invalid")
    try
      val pidFile = tempDir.resolve("test.pid")
      Files.writeString(pidFile, "not-a-number")

      val result = DaemonManager.readPidFile(pidFile)
      result.isLeft shouldBe true
      result.swap.getOrElse("") should include("Failed to read")

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "DaemonManager.isProcessRunning" should "detect current process as running" in {
    val currentPid = DaemonManager.currentPid
    DaemonManager.isProcessRunning(currentPid) shouldBe true
  }

  it should "return false for non-existent PID" in {
    // Use a very high PID that's unlikely to exist
    val nonExistentPid = 999999999L
    DaemonManager.isProcessRunning(nonExistentPid) shouldBe false
  }

  it should "handle invalid PID gracefully" in {
    DaemonManager.isProcessRunning(-1L) shouldBe false
    DaemonManager.isProcessRunning(0L) shouldBe false
  }

  "DaemonManager.isDaemonRunning" should "detect running process from PID file" in {
    val tempDir = Files.createTempDirectory("daemon-test-running")
    try
      val pidFile = tempDir.resolve("test.pid")
      val currentPid = DaemonManager.currentPid
      Files.writeString(pidFile, currentPid.toString)

      DaemonManager.isDaemonRunning(pidFile) shouldBe true

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "return false when process not running" in {
    val tempDir = Files.createTempDirectory("daemon-test-notrunning")
    try
      val pidFile = tempDir.resolve("test.pid")
      Files.writeString(pidFile, "999999999")

      DaemonManager.isDaemonRunning(pidFile) shouldBe false

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "return false when PID file doesn't exist" in {
    val tempDir = Files.createTempDirectory("daemon-test-nopid")
    try
      val pidFile = tempDir.resolve("nonexistent.pid")
      DaemonManager.isDaemonRunning(pidFile) shouldBe false

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "DaemonManager.removeIfStale" should "remove stale PID file" in {
    val tempDir = Files.createTempDirectory("daemon-test-stale")
    try
      val pidFile = tempDir.resolve("test.pid")
      Files.writeString(pidFile, "999999999") // Non-existent PID

      val result = DaemonManager.removeIfStale(pidFile)
      result shouldBe Right(true)
      Files.exists(pidFile) shouldBe false

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "not remove PID file for running process" in {
    val tempDir = Files.createTempDirectory("daemon-test-active")
    try
      val pidFile = tempDir.resolve("test.pid")
      val currentPid = DaemonManager.currentPid
      Files.writeString(pidFile, currentPid.toString)

      val result = DaemonManager.removeIfStale(pidFile)
      result.isLeft shouldBe true
      result.swap.getOrElse("") should include("still running")
      Files.exists(pidFile) shouldBe true

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  it should "return false when no PID file exists" in {
    val tempDir = Files.createTempDirectory("daemon-test-nopidstale")
    try
      val pidFile = tempDir.resolve("nonexistent.pid")
      val result = DaemonManager.removeIfStale(pidFile)

      result shouldBe Right(false)

    finally
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  }

  "DaemonManager.installShutdownHook" should "install hook without error" in {
    var cleanupCalled = false
    val cleanup = () => {
      cleanupCalled = true
    }

    // This should not throw
    noException should be thrownBy {
      DaemonManager.installShutdownHook(cleanup)
    }

    // Note: We can't easily test that the hook actually runs on shutdown
    // in a unit test without terminating the JVM
  }

  it should "handle cleanup exceptions gracefully" in {
    val cleanup = () => {
      throw new RuntimeException("Test exception")
    }

    // Hook installation should still succeed even if cleanup would throw
    noException should be thrownBy {
      DaemonManager.installShutdownHook(cleanup)
    }
  }
