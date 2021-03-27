package io.github.rpiotrow.advent2020.day13

import io.github.rpiotrow.advent2020.Input
import io.github.rpiotrow.advent2020.Solution
import zio._

object ShuttleSearch {
  val solution: Solution =
    for {
      lines <- Input.readLines("day13.input").runCollect.map(_.toList)
      state <- BusShuttle.parse(lines)
      multiplication = state.earliestDeparture.multiplication
      _ <- console.putStrLn(s"The ID of the earliest bus multiplied by the number of minutes is $multiplication")
    } yield (multiplication, -1L)
}
