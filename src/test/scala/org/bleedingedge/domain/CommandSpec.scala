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

package org.bleedingedge.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

class CommandSpec extends AnyFlatSpec with Matchers:

  // Helper to create temporary test directory
  def withTempDir(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("command-spec-")
    try
      test(tempDir)
    finally
      // Clean up
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)

  "MoveCommand" should "move a file correctly (BUG FIX TEST)" in {
    withTempDir { tempDir =>
      // Create source file
      val sourceFile = tempDir.resolve("source.txt")
      Files.write(sourceFile, "test content".getBytes)

      // The bug was using 'from' twice instead of 'from' and 'to'
      // This test verifies the fix works correctly
      val command = MoveCommand("source.txt", "destination.txt")
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
      Files.exists(tempDir.resolve("destination.txt/source.txt")) shouldBe true
    }
  }

  "DeleteCommand" should "delete an existing file" in {
    withTempDir { tempDir =>
      // Create file to delete
      val file = tempDir.resolve("to-delete.txt")
      Files.write(file, "content".getBytes)

      val command = DeleteCommand("to-delete.txt")
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
      Files.exists(file) shouldBe false
    }
  }

  it should "succeed even if file doesn't exist" in {
    withTempDir { tempDir =>
      val command = DeleteCommand("nonexistent.txt")
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
    }
  }

  "CreateCommand" should "create a new file with content" in {
    withTempDir { tempDir =>
      val content = "new file content".getBytes
      val command = CreateCommand("newfile.txt", () => content)
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
      val createdFile = tempDir.resolve("newfile.txt")
      Files.exists(createdFile) shouldBe true
      Files.readAllBytes(createdFile) shouldBe content
    }
  }

  it should "fail if file already exists" in {
    withTempDir { tempDir =>
      // Create existing file
      val file = tempDir.resolve("existing.txt")
      Files.write(file, "existing content".getBytes)

      val command = CreateCommand("existing.txt", () => "new content".getBytes)
      val result = command.apply(tempDir)

      result.isFailure shouldBe true
    }
  }

  "UpdateCommand" should "update an existing file" in {
    withTempDir { tempDir =>
      // Create initial file
      val file = tempDir.resolve("update-me.txt")
      Files.write(file, "old content".getBytes)

      val newContent = "updated content".getBytes
      val command = UpdateCommand("update-me.txt", () => newContent)
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
      Files.readAllBytes(file) shouldBe newContent
    }
  }

  it should "create file if it doesn't exist" in {
    withTempDir { tempDir =>
      val content = "new content".getBytes
      val command = UpdateCommand("new-file.txt", () => content)
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
      val file = tempDir.resolve("new-file.txt")
      Files.exists(file) shouldBe true
      Files.readAllBytes(file) shouldBe content
    }
  }

  "DoNothingCommand" should "always succeed without side effects" in {
    withTempDir { tempDir =>
      val command = DoNothingCommand
      val result = command.apply(tempDir)

      result.isSuccess shouldBe true
    }
  }

  "Command factory" should "create DoNothingCommand when states are identical" in {
    val state1 = LocationState("/file", "content".getBytes)
    val state2 = LocationState("/file", "content".getBytes)

    val command = Command(state1, state2)

    command shouldBe DoNothingCommand
  }

  it should "create MoveCommand when content is same but location differs" in {
    val bytes = "same content".getBytes
    val state1 = LocationState("/file1", bytes)
    val state2 = LocationState("/file2", bytes)

    val command = Command(state1, state2)

    command shouldBe a[MoveCommand]
    val moveCmd = command.asInstanceOf[MoveCommand]
    moveCmd.from shouldBe "/file1"
    moveCmd.to shouldBe "/file2"
  }

  it should "create CreateCommand when earlier state is empty" in {
    val state1 = LocationState("/file", Array.empty[Byte])
    val state2 = LocationState("/file", "new content".getBytes)

    val command = Command(state1, state2)

    command shouldBe a[CreateCommand]
  }

  it should "create DeleteCommand when later state is empty" in {
    val state1 = LocationState("/file", "content".getBytes)
    val state2 = LocationState("/file", Array.empty[Byte])

    val command = Command(state1, state2)

    command shouldBe a[DeleteCommand]
  }

  it should "create UpdateCommand when content changes at same location" in {
    val state1 = LocationState("/file", "old content".getBytes)
    val state2 = LocationState("/file", "new content".getBytes)

    val command = Command(state1, state2)

    command shouldBe an[UpdateCommand]
  }

  it should "create DoNothingCommand when states are unrelated" in {
    val state1 = LocationState("/file1", "content1".getBytes)
    val state2 = LocationState("/file2", "content2".getBytes)

    val command = Command(state1, state2)

    command shouldBe DoNothingCommand
  }
