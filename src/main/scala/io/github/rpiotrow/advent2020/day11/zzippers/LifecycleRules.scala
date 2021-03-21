package io.github.rpiotrow.advent2020.day11.zzippers

import io.github.rpiotrow.advent2020.day11.{Cell, EmptySeat, Floor, OccupiedSeat}
import zio._

object LifecycleRules {
  type LifecycleRulesType = GridZZipper[Cell] => UIO[Cell]

  val neighboursRules: LifecycleRulesType = (g: GridZZipper[Cell]) =>
    for {
      n <- g.getNeighbors
      cell = changeCell(g, n.count(_ == OccupiedSeat), 4)
    } yield cell

  val visibilityRules: LifecycleRulesType = (g: GridZZipper[Cell]) =>
    for {
      n <- g.getVisibles(Floor)
      cell = changeCell(g, n.count(_ == OccupiedSeat), 5)
    } yield cell

  private def changeCell(g: GridZZipper[Cell], occupied: Long, occupiedLimit: Int) =
    g.extract match {
      case EmptySeat if occupied == 0 =>
        OccupiedSeat
      case OccupiedSeat if occupied >= occupiedLimit =>
        EmptySeat
      case p => p
    }
}
