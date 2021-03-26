package io.github.rpiotrow.advent2020.day12

import io.github.rpiotrow.advent2020.day12.NavigationInstructionProcessor.processInstructions
import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.blocking.Blocking
import zio.stream.ZStream
import zio._

object RainRisk {
  val solution: Solution = {
    val directShip = DirectShip(Location.zero, East)
    val shipWithWaypoint = ShipWithWaypoint(Location.zero, Waypoint(10, 1))
    readInstructions.broadcast(2, 10).use {
      case streamCopy1::streamCopy2::Nil =>
        for {
          fiber1 <- processInstructions(directShip, streamCopy1).fork
          fiber2 <- processInstructions(shipWithWaypoint, streamCopy2).fork
          processedDirectly     <- fiber1.join
          processedWithWaypoint <- fiber2.join
          distanceText = "The Manhattan distance between starting and current ship locations"
          distanceDirectly = processedDirectly.location.manhattanDistanceToZero
          distanceWithWaypoint = processedWithWaypoint.location.manhattanDistanceToZero
          _ <- console.putStrLn(s"$distanceText directly is $distanceDirectly")
          _ <- console.putStrLn(s"$distanceText with waypoint is $distanceWithWaypoint")
        } yield (distanceDirectly, distanceWithWaypoint)
      case _ => ZIO.dieMessage("impossible")
    }
  }

  private def readInstructions: ZStream[Blocking, String, NavigationInstruction] =
    Input
      .readLines("day12.input")
      .mapM(NavigationInstruction.parse)
}
