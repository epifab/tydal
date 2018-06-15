name := "tagless-dal"
organization := "epifab.io"
version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Ypartial-unification")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.postgresql" % "postgresql" % "42.2.2",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
