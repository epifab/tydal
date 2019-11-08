import sbt.Global

/*****************************************************************
 Global Settings & Configs
  *****************************************************************/
Global / name := "tydal"
Global / version := sys.env.getOrElse("BUILD_NUMBER", "LOCAL")
Global / organization := "epifab.io"
Global / scalaVersion := "2.13.1"
Global / exportJars := true
Global / parallelExecution := false
Global / scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-language:existentials",
  "-language:higherKinds",
  // "-Xlint:_,-missing-interpolator",
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.postgresql" % "postgresql" % "42.2.2",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
