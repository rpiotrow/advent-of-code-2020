package io.github.rpiotrow.advent2020.day13

import cats.data.NonEmptyList
import io.github.rpiotrow.advent2020.Input
import io.github.rpiotrow.advent2020.Solution
import zio._
import zio.stream.ZStream

object ShuttleSearch {
  val solution: Solution =
    for {
      lines <- Input.readLines("day13.input").runCollect.map(_.toList)
      state <- BusShuttle.parse(lines)
      multiplication = state.earliestDeparture.multiplication
      _ <- console.putStrLn(s"The ID of the earliest bus multiplied by the number of minutes is $multiplication")
      contestInput <- parseContest(lines)
      solution = Contest.compute(contestInput.head.busId, contestInput.tail)
      _ <- console.putStrLn(s"Solution of operator contest is $solution")
    } yield (multiplication, solution)

  private def parseContest(lines: List[String]): IO[String, NonEmptyList[ContestBusRequirement]] =
    lines match {
      case _::buses::Nil => ContestBusRequirement.parseList(buses)
      case _             => ZIO.fail("invalid input")
    }
}
