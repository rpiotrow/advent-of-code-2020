package io.github.rpiotrow.advent2020.day11

import cats.implicits._
import io.github.rpiotrow.advent2020.day11.LifecycleRules.LifecycleRulesType
import io.github.rpiotrow.advent2020.day11.UnorderedFoldableWithInitValue._
import io.github.rpiotrow.advent2020.day11.WaitingArea.OccupiedCountAcc
import zio._
import zio.stream.ZStream

case class WaitingArea(grid: GridZipper[Cell]) {

  def occupiedCountWhenStable(lifecycleRules: LifecycleRulesType): IO[String, Int] =
    ZStream
      .iterate(this)(_.nextGeneration(lifecycleRules))
      .mapAccum(OccupiedCountAcc(0, this.occupiedCount))((acc, waitingArea) => {
        val x = OccupiedCountAcc(previous = acc.current, current = waitingArea.occupiedCount)
        (x, x)
      })
      .takeUntil(acc => acc.previous > 0 && acc.current == acc.previous)
      .runLast
      .flatMap(ZIO.fromOption(_).orElseFail("solution not found"))
      .map(_.current)

  def print(header: String): WaitingArea = {
    println(header)
    println(grid.show)
    this
  }

  private def occupiedCount: Int =
    grid.unorderedFold(0)((acc, cell) => if (cell == OccupiedSeat) acc+1 else acc)

  private def nextGeneration(lifecycleRules: LifecycleRulesType): WaitingArea =
    WaitingArea(grid.coflatMap(lifecycleRules))
}

object WaitingArea {

  private case class OccupiedCountAcc(previous: Int, current: Int)

}