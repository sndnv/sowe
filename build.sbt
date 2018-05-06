import sbt.Keys._

name in ThisBuild := "owe"
licenses in ThisBuild := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
homepage in ThisBuild := Some(url("https://github.com/sndnv/sowe"))

scalaVersion in ThisBuild := "2.12.6"

lazy val akkaVersion = "2.5.12"

lazy val owe = (project in file("."))
  .settings(
    crossScalaVersions := Seq("2.12.6"),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "org.scalacheck"    %% "scalacheck" % "1.14.0"    % Test,
      "org.scalatest"     %% "scalatest"  % "3.0.5"     % Test
    ),
    logBuffered in Test := false,
    parallelExecution in Test := false,
    wartremoverWarnings ++= Warts.unsafe
  )
