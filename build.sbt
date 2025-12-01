name := "bleeding-edge"
version := "3.0.0"
scalaVersion := "3.6.2"

// Enable native packager plugins
enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)
enablePlugins(LinuxPlugin)
enablePlugins(DebianPlugin)
enablePlugins(RpmPlugin)
enablePlugins(WindowsPlugin)

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.5.8",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "com.typesafe" % "config" % "1.4.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// Compiler options for Scala 3
scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xfatal-warnings"
)

// Main class
val mainClassName = "org.bleedingedge.cli.BleedingEdgeCLI"
Compile / mainClass := Some(mainClassName)

// Common packaging metadata
val packageMaintainer = "Miles <dm@iondrive.co>"
val packageSummaryText = "Distributed P2P file synchronization"
val packageDescriptionText = "Modern distributed peer-to-peer file synchronization system built with Scala 3"

// Assembly settings for fat JAR
assembly / assemblyJarName := s"${name.value}-assembly-${version.value}.jar"
assembly / mainClass := Some(mainClassName)

// Merge strategy for assembly
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "reference.conf" | "application.conf" => MergeStrategy.concat
  case x if x.contains("module-info") => MergeStrategy.discard
  case _ => MergeStrategy.first
}

// Universal packaging
Universal / maintainer := packageMaintainer

// Debian packaging
Debian / maintainer := packageMaintainer
Debian / debianPackageDependencies := Seq("openjdk-17-jre | openjdk-17-jre-headless")

// RPM packaging
Rpm / maintainer := packageMaintainer
Rpm / rpmVendor := "BleedingEdge Project"
Rpm / rpmLicense := Some("BSD-2-Clause")

// Windows packaging
Windows / maintainer := "Miles"

// Docker settings
Docker / packageName := "bleeding-edge"
Docker / dockerBaseImage := "eclipse-temurin:17-jre-alpine"
Docker / dockerExposedPorts := Seq(8888, 4446)
Docker / dockerRepository := Some("bleedingedge")

// Exclude packaging metadata keys from lintUnused check (they're used by packaging commands)
Global / excludeLintKeys ++= Set(
  Docker / dockerBaseImage,
  Docker / dockerExposedPorts,
  Docker / dockerRepository,
  Rpm / rpmVendor,
  Rpm / rpmLicense
)
