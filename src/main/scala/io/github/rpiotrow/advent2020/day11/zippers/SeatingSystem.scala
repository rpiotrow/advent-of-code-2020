package io.github.rpiotrow.advent2020.day11.zippers

import io.github.rpiotrow.advent2020.day11.Cell
import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.blocking.Blocking
import zio.{ZIO, console}

object SeatingSystem {

  val solution: Solution =
    for {
      waitingArea <- readWaitingArea
      firstCount <- waitingArea.occupiedCountWhenStable(LifecycleRules.neighboursRules)
      secondCount <- waitingArea.occupiedCountWhenStable(LifecycleRules.visibilityRules)
      _ <- console.putStrLn(s"$firstCount seats end up occupied when following first rules")
      _ <- console.putStrLn(s"$secondCount seats end up occupied when following second rules")
    } yield (firstCount, secondCount)

  private def readWaitingArea: ZIO[Blocking, String, WaitingArea] =
    Input
      .readLines("day11.input")
      .mapM(Cell.listFromString)
      .runCollect
      .flatMap(c => GridZipper.fromList(c.toList))
      .map(WaitingArea(_))
}
