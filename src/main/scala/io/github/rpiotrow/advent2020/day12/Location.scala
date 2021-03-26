package io.github.rpiotrow.advent2020.day12

case class Location(east: Int, north: Int) extends WithCoordinates {
  def manhattanDistanceToZero: Int = Math.abs(east) + Math.abs(north)

  def move: (MoveDirection, Int) => Location = moveApply(Location.apply)
}

object Location {
  val zero = Location(0, 0)
}
