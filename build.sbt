import sbt.Global

/*****************************************************************
 Global Settings & Configs
  *****************************************************************/
name := "tydal"
version := "1.3.7"
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
  "org.typelevel" %% "cats-core" % "2.4.2",
  "org.typelevel" %% "cats-effect" % "2.3.3",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-parser" % "0.13.0",
  "org.postgresql" % "postgresql" % "42.2.2",
  "com.zaxxer" % "HikariCP" % "3.4.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)
