package io.github.rpiotrow.advent2020.day12

case class Waypoint(east: Int, north: Int) extends WithCoordinates {
  def move: (MoveDirection, Int) => Waypoint = moveApply(Waypoint.apply)

  def rotate(direction: TurnDirection, amount: TurnAmount): Waypoint =
    direction match {
      case LeftDirection => rotateLeft(amount)
      case RightDirection => rotateRight(amount)
    }

  private def rotateLeft(amount: TurnAmount): Waypoint =
    amount match {
      case Degrees90 => Waypoint(-north, east)
      case Degrees180 => Waypoint(-east, -north)
      case Degrees270 => Waypoint(north, -east)
    }

  private def rotateRight(amount: TurnAmount): Waypoint =
    amount match {
      case Degrees90 => Waypoint(north, -east)
      case Degrees180 => Waypoint(-east, -north)
      case Degrees270 => Waypoint(-north, east)
    }
}
