package io.github.rpiotrow.advent2020

import scopt.OParser
import zio._

case class Config(day: Option[Int] = None)

object Config {
  private val configBuilder = OParser.builder[Config]
  val parser = {
    import configBuilder._
    OParser.sequence(
      programName("advent-of-code-2020"),
      opt[Int]('d', "day")
        .action((x, c) => c.copy(day = Some(x)))
        .text("what day you want to run"),
    )
  }
}

object Main extends zio.App {

  private val days: Map[Int, Solution] = Map(
    1 -> day01.ReportRepair.solution,
    2 -> day02.PasswordPhilosophy.solution,
    3 -> day03.TobogganTrajectory.solution,
    4 -> day04.PassportProcessing.solution,
    5 -> day05.BinaryBoarding.solution,
    6 -> day06.CustomCustoms.solution,
    7 -> day07.HandyHaversacks.solution
  )

  private def solution(day: Int): ZIO[ZEnv, String, Unit] = {
    for {
      _ <- console.putStrLnErr(s"Day: $day")
      _ <- days.getOrElse(day, ZIO.fail("There is no such day!!!"))
    } yield ()
  }

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (OParser.parse(Config.parser, args, Config()) match {
      case Some(config) =>
        config.day.fold(ZIO.collectAll(days.keys.map(solution)).map(_ => ()))(solution)
      case _ =>
        ZIO.fail("Invalid parameters!!!")
    }).foldM(
      err => console.putStrLn(s"Execution failed: $err") *> IO.succeed(ExitCode.failure),
      _   => IO.succeed(ExitCode.success)
    )

}
