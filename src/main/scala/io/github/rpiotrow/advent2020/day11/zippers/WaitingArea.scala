package io.github.rpiotrow.advent2020.day11.zippers

import cats.implicits._
import io.github.rpiotrow.advent2020.day11.{Cell, OccupiedSeat}
import io.github.rpiotrow.advent2020.day11.zippers.Countable.CountableOps
import io.github.rpiotrow.advent2020.day11.zippers.LifecycleRules.LifecycleRulesType
import io.github.rpiotrow.advent2020.day11.zippers.WaitingArea.OccupiedCountAcc
import zio._
import zio.stream.ZStream

case class WaitingArea(grid: GridZipper[Cell]) {

  def occupiedCountWhenStable(lifecycleRules: LifecycleRulesType): IO[String, Int] =
    ZStream
      .unfold(this)(g => {
        val x = g.nextGeneration(lifecycleRules);
        Some((x, x))
      })
//      .iterate(this)(_.nextGeneration(lifecycleRules))
      .mapAccum(OccupiedCountAcc(0, this.occupiedCount))((acc, waitingArea) => {
        val x = OccupiedCountAcc(previous = acc.current, current = waitingArea.occupiedCount)
        (x, x)
      })
      .map({
        case o @ OccupiedCountAcc(a, b) => {
          println(s"a = $a b = $b")
          o
        }
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

  private def occupiedCount: Int = grid.count(_ == OccupiedSeat)

  private def nextGeneration(lifecycleRules: LifecycleRulesType): WaitingArea =
    WaitingArea(grid.coflatMap(lifecycleRules)).print("")
}

object WaitingArea {

  private case class OccupiedCountAcc(previous: Int, current: Int)

}