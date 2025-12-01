package org.bleedingedge.cli

import com.typesafe.scalalogging.LazyLogging
import org.bleedingedge.infrastructure.scheduling.Scheduler
import org.bleedingedge.network.NetworkManager
import org.bleedingedge.sync.SyncManager

import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/**
 * Command-line interface for BleedingEdge distributed file synchronization.
 *
 * Provides commands for initializing, starting, stopping, and managing sync operations.
 *
 * @since 3.0.0
 */
object BleedingEdgeCLI extends LazyLogging:

  private val VERSION = "3.0.0"
  private val DEFAULT_PORT = 0  // 0 = OS assigns random ephemeral port

  /**
   * Main entry point for the CLI.
   */
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      printUsage()
      System.exit(1)

    val command = args(0).toLowerCase
    val commandArgs = args.drop(1)

    command match
      case "sync" => syncCommand(commandArgs)
      case "version" => versionCommand()
      case "help" => printUsage()
      case _ =>
        println(s"Unknown command: $command")
        printUsage()
        System.exit(1)

  /**
   * Prints usage information.
   */
  private def printUsage(): Unit =
    println(s"""
      |BleedingEdge v$VERSION - Distributed File Synchronization
      |
      |Usage: bleedingedge <command> [options]
      |
      |Commands:
      |  sync <directory>           Start syncing a directory
      |  version                    Show version information
      |  help                       Show this help message
      |
      |Options:
      |  --port <port>             Network port to use (default: 0 = OS-assigned)
      |  --no-discovery            Disable automatic peer discovery
      |  --peer <host:port>        Connect to a specific peer
      |
      |Examples:
      |  bleedingedge sync ~/Documents
      |  bleedingedge sync ~/Documents --port 9000
      |  bleedingedge sync ~/Documents --peer 192.168.1.100:8888
      |
      |For more information, visit: https://github.com/bleedingedge/bleedingedge
      |""".stripMargin)

  /**
   * Primary sync command - starts syncing a directory.
   * This is the main user-facing command for ease of use.
   */
  private def syncCommand(args: Array[String]): Unit =
    if args.isEmpty then
      println("✗ No directory specified")
      println("Usage: bleedingedge sync <directory>")
      System.exit(1)

    // Start syncing directly (no initialization needed)
    startCommand(args)

  /**
   * Start syncing a directory.
   */
  private def startCommand(args: Array[String]): Unit =
    if args.isEmpty then
      println("✗ No directory specified")
      println("Usage: bleedingedge start <directory>")
      System.exit(1)

    val directory = Paths.get(args(0))

    // Ensure directory exists
    if !Files.exists(directory) then
      println(s"✗ Directory does not exist: $directory")
      System.exit(1)

    // Parse options
    var port = DEFAULT_PORT
    var enableDiscovery = true
    var manualPeers = List.empty[String]

    var i = 1
    while i < args.length do
      args(i) match
        case "--port" if i + 1 < args.length =>
          port = args(i + 1).toInt
          i += 2
        case "--no-discovery" =>
          enableDiscovery = false
          i += 1
        case "--peer" if i + 1 < args.length =>
          manualPeers = manualPeers :+ args(i + 1)
          i += 2
        case other =>
          println(s"✗ Unknown option: $other")
          System.exit(1)

    println(s"Starting BleedingEdge sync...")
    println(s"  Directory: $directory")
    if port == 0 then
      println(s"  Port: (OS will assign)")
    else
      println(s"  Port: $port")
    println(s"  Discovery: ${if enableDiscovery then "enabled" else "disabled"}")
    if manualPeers.nonEmpty then
      println(s"  Manual peers: ${manualPeers.mkString(", ")}")

    Try {
      // Need enough threads for: network-accept, peer-discovery-listen, peer-discovery-broadcast,
      // file-monitor (all blocking), plus peer-connect, peer-send, peer-receive tasks
      val scheduler = Scheduler(10)
      given scala.concurrent.ExecutionContext = scheduler.executionContext

      // Create sync manager
      val syncManager = SyncManager(directory, scheduler)

      // Create network manager with sync manager's message handler
      val networkManager = new NetworkManager(port, scheduler, syncManager.handleMessage)

      // Connect sync manager to network manager
      syncManager.connect(networkManager)

      // Start network manager
      Await.result(networkManager.start(enableDiscovery = enableDiscovery), 10.seconds)

      // Show actual bound port if OS-assigned
      if port == 0 then
        println(s"  Actual port: ${networkManager.getActualPort}")

      // Start sync manager
      Await.result(syncManager.start(), 10.seconds)

      // Connect to manual peers
      for peerAddress <- manualPeers do
        val Array(host, peerPort) = peerAddress.split(":")
        val peerInfo = org.bleedingedge.network.PeerInfo(host, peerPort.toInt)
        Await.result(networkManager.connectToPeer(peerInfo), 10.seconds)
        println(s"✓ Connected to peer: $peerAddress")

      println(s"✓ Sync started successfully")
      println(s"\nBleedingEdge is now running. Press Ctrl+C to stop.")

      // Keep running until interrupted
      try
        while true do
          Thread.sleep(1000)
      catch
        case _: InterruptedException =>
          println("\nShutting down...")
          syncManager.stop()
          networkManager.stop()
          scheduler.gracefulShutdown(5)
          println("✓ Shutdown complete")

    } match
      case Success(_) => System.exit(0)
      case Failure(e) =>
        e match
          case _: java.net.BindException =>
            println(s"✗ Failed to start: Port $port is already in use")
            println(s"  This usually means:")
            println(s"  1. Another instance of BleedingEdge is already running")
            println(s"  2. Another application is using port $port")
            println(s"\n  Solutions:")
            println(s"  - Stop the other instance or application")
            println(s"  - Use a different port: bleedingedge sync $directory --port 9000")
          case _ =>
            println(s"✗ Failed to start sync: ${e.getMessage}")
        logger.error("Start failed", e)
        System.exit(1)

  /**
   * Show version information.
   */
  private def versionCommand(): Unit =
    println(s"BleedingEdge v$VERSION")
    println("Distributed File Synchronization")
    println("Copyright (c) 2025 Miles Hampson")
    System.exit(0)
