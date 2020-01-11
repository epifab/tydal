import sbt.Global

/*****************************************************************
 Global Settings & Configs
  *****************************************************************/
name := "tydal"
version := "1.2.4"
organization := "epifab.io"

exportJars := true
parallelExecution := false

scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.13.1")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-language:existentials",
  "-language:higherKinds",
  "-Ystatistics:typer",
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "io.circe" %% "circe-core" % "0.12.3",
  "io.circe" %% "circe-generic" % "0.12.3",
  "io.circe" %% "circe-parser" % "0.12.3",
  "org.postgresql" % "postgresql" % "42.2.2",
  "com.zaxxer" % "HikariCP" % "3.4.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)
