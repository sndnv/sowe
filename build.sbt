import sbt.Keys._

name in ThisBuild := "owe"
licenses in ThisBuild := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
homepage in ThisBuild := Some(url("https://github.com/sndnv/sowe"))

scalaVersion in ThisBuild := "2.12.6"

lazy val akkaVersion = "2.5.14"

lazy val commonSettings = Seq(
  crossScalaVersions := Seq("2.12.6"),
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor"   % akkaVersion,
    "org.scalacheck"    %% "scalacheck"   % "1.14.0"    % Test,
    "org.scalatest"     %% "scalatest"    % "3.0.5"     % Test,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
  ),
  logBuffered in Test := false,
  parallelExecution in Test := false,
  wartremoverWarnings in (Compile, compile) ++= Warts.unsafe,
  scalacOptions := Seq("-unchecked", "-deprecation")
)

lazy val owe = (project in file("."))
  .settings(commonSettings)

lazy val `owe-debug-ui` = (project in file("owe-debug-ui"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "org.webjars" % "jquery" % "3.3.1-1",
      "com.typesafe.play" %% "play-json" % "2.7.0-M1",
      "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
    )
  )
  .enablePlugins(PlayScala)
  .dependsOn(owe)

addCommandAlias("qa", "; clean; coverage; test; coverageReport")
