package io.github.rpiotrow.advent2020.day11

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.blocking.Blocking
import zio._

object SeatingSystem {

  val solution: Solution =
    for {
      waitingArea <- readWaitingArea
      firstCount  <- waitingArea.occupiedCountWhenStable
      _ <- console.putStrLn(s"$firstCount seats end up occupied when following first rules")
      _ <- console.putStrLn(s"??? seats end up occupied when following second rules")
    } yield (firstCount, 0L)

  private def readWaitingArea: ZIO[Blocking, String, WaitingArea] =
    Input
      .readLines("day11.input")
      .mapM(Cell.listFromString)
      .runCollect
      .flatMap(c => GridZipper.fromList(c.toList))
      .map(WaitingArea(_))
}
