import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.rpiotrow"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2020",
    libraryDependencies ++= Seq(zio, zioStreams)
  )