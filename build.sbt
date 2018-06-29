import sbt.Keys._

name in ThisBuild := "owe"
licenses in ThisBuild := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
homepage in ThisBuild := Some(url("https://github.com/sndnv/sowe"))

scalaVersion in ThisBuild := "2.12.6"

lazy val akkaVersion = "2.5.13"

lazy val owe = (project in file("."))
  .settings(
    crossScalaVersions := Seq("2.12.6"),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor"       % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "org.scalacheck"    %% "scalacheck"       % "1.14.0"    % Test,
      "org.scalatest"     %% "scalatest"        % "3.0.5"     % Test,
      "com.typesafe.akka" %% "akka-testkit"     % akkaVersion % Test
    ),
    logBuffered in Test := false,
    parallelExecution in Test := false,
    wartremoverWarnings in (Compile, compile) ++= Warts.unsafe,
    scalacOptions := Seq("-unchecked", "-deprecation")
  )

addCommandAlias("qa", "; clean; coverage; test; coverageReport")
