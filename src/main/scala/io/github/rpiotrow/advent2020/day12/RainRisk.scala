package io.github.rpiotrow.advent2020.day12

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.blocking.Blocking
import zio.stream.ZStream
import zio._

object RainRisk {
  val solution: Solution =
    for {
      processed <- processInstructions(Ship(ShipLocation.zero, East), readInstructions)
      distance = processed.location.manhattanDistanceToZero
      _ <- console.putStrLn(s"The Manhattan distance between starting and current ship locations is $distance")
      _ <- console.putStrLn(s"???")
    } yield (distance, -1L)

  private def readInstructions: ZStream[Blocking, String, NavigationInstruction] =
    Input
      .readLines("day12.input")
      .mapM(NavigationInstruction.parse)

  private def processInstructions(
     ship: Ship,
     stream: ZStream[Blocking, String, NavigationInstruction]
  ): ZIO[Blocking, String, Ship] =
    stream.fold(ship)((ship, instruction) => ship.navigate(instruction))

}
