package io.github.rpiotrow.advent2020.day11

import zio._

sealed trait Cell

case object Floor extends Cell {
  override def toString: String = "."
}
case object EmptySeat extends Cell {
  override def toString: String = "L"
}
case object OccupiedSeat extends Cell {
  override def toString: String = "#"
}

object Cell {
  def listFromString(s: String): IO[String, List[Cell]] =
    ZIO.foreach(s.toList)(fromChar)

  private def fromChar(c: Char) = c match {
    case 'L' => ZIO.succeed(EmptySeat)
    case '#' => ZIO.succeed(OccupiedSeat)
    case '.' => ZIO.succeed(Floor)
    case _ => ZIO.fail("invalid input")
  }
}