import sbt._

object Dependencies {
  lazy val zio = "dev.zio" %% "zio" % "1.0.3"
  lazy val zioStreams = "dev.zio" %% "zio-streams" % "1.0.3"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"
}
