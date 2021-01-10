package io.github.rpiotrow.advent2020.day11

import cats.implicits._

object LifecycleRules {
  type LifecycleRulesType = GridZipper[Cell] => Cell

  val neighboursRules: LifecycleRulesType = (g: GridZipper[Cell]) =>
    changeCell(g, g.getNeighbors.count(_ == OccupiedSeat), 4)

  val visibilityRules: LifecycleRulesType = (g: GridZipper[Cell]) =>
    changeCell(g, g.getVisibles(Floor).count(_ == OccupiedSeat), 5)

  private def changeCell(g: GridZipper[Cell], occupied: Int, occupiedLimit: Int) =
    g.extract match {
      case EmptySeat if occupied == 0 =>
        OccupiedSeat
      case OccupiedSeat if occupied >= occupiedLimit =>
        EmptySeat
      case p => p
    }
}
