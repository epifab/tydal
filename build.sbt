import sbt.Global

/*****************************************************************
 Global Settings & Configs
  *****************************************************************/
name := "tydal"
version := "1.3.11"
organization := "epifab.io"

exportJars := true
parallelExecution := false

scalaVersion := "2.13.8"
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
  "org.typelevel" %% "cats-core" % "2.7.0",
  "org.typelevel" %% "cats-effect" % "3.3.8",
  "com.chuusai" %% "shapeless" % "2.3.9",
  "io.circe" %% "circe-core" % "0.14.1",
  "io.circe" %% "circe-generic" % "0.14.1",
  "io.circe" %% "circe-parser" % "0.14.1",
  "org.postgresql" % "postgresql" % "42.3.3",
  "com.zaxxer" % "HikariCP" % "5.0.1",
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)
