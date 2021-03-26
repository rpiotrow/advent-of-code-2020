package io.github.rpiotrow.advent2020.day12

trait WithCoordinates {
  val east: Int
  val north: Int

  def moveApply[A <: WithCoordinates](apply: (Int, Int) => A)(direction: MoveDirection, amount: Int): A = {
    direction match {
      case North => apply(east, north + amount)
      case South => apply(east, north - amount)
      case East => apply(east + amount, north)
      case West => apply(east - amount, north)
    }
  }
}
