import sbt._
import Keys._

object Build extends sbt.Build {
  lazy val project = Project("Shelley", file("."))
    .settings(
      organization := "mary",
      version := "0.1",
      scalaVersion := "2.9.1",
      scalacOptions := Seq("-deprecation", "-unchecked", "-encoding", "utf8"),
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "1.12.2" % "test"
      ))
}