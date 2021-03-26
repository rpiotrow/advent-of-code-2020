package io.github.rpiotrow.advent2020.day12

trait Ship {
  def location: Location
  def navigate(instruction: NavigationInstruction): Ship
}

case class DirectShip(location: Location, moveDirection: MoveDirection) extends Ship {
  override def navigate(instruction: NavigationInstruction): DirectShip =
    instruction match {
      case MoveForward(amount) =>
        copy(location = location.move(moveDirection, amount))
      case Move(direction, amount) =>
        copy(location = location.move(direction, amount))
      case Turn(direction, amount) =>
        copy(moveDirection = moveDirection.change(direction, amount))
    }
}

case class ShipWithWaypoint(location: Location, waypoint: Waypoint) extends Ship {
  override def navigate(instruction: NavigationInstruction): ShipWithWaypoint =
    instruction match {
      case MoveForward(amount) =>
        copy(location = Location(
          east = location.east + waypoint.east * amount,
          north = location.north + waypoint.north * amount
        ))
      case Move(direction, amount) =>
        copy(waypoint = waypoint.move(direction, amount))
      case Turn(direction, amount) =>
        copy(waypoint = waypoint.rotate(direction, amount))
    }
}
