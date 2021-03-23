package io.github.rpiotrow.advent2020.day12

case class ShipLocation(east: Int, north: Int) {
  def manhattanDistanceToZero: Int =
    Math.abs(east) + Math.abs(north)

  def move(direction: MoveDirection, amount: Int): ShipLocation =
    direction match {
      case North => copy(north = north + amount)
      case South => copy(north = north - amount)
      case East => copy(east = east + amount)
      case West => copy(east = east - amount)
    }
}

object ShipLocation {
  val zero = ShipLocation(0, 0)
}
