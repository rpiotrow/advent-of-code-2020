import sbt._

object Versions {
  val zio        = "1.0.5"
  val cats       = "2.2.0"
}

object Dependencies {
  lazy val zio        = "dev.zio" %% "zio" % Versions.zio
  lazy val zioStreams = "dev.zio" %% "zio-streams" % Versions.zio
  lazy val cats       = "org.typelevel" %% "cats-core" % Versions.cats

  lazy val scopt      = "com.github.scopt" %% "scopt" % "4.0.0"
}

object TestDependencies {
  lazy val zioTest    = "dev.zio" %% "zio-test"          % Versions.zio % "test"
  lazy val zioTestSbt = "dev.zio" %% "zio-test-sbt"      % Versions.zio % "test"
}