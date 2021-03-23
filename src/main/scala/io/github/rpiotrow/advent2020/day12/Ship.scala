package io.github.rpiotrow.advent2020.day12

case class Ship(location: ShipLocation, moveDirection: MoveDirection) {
  def navigate(instruction: NavigationInstruction): Ship =
    instruction match {
      case MoveForward(amount) =>
        copy(location = location.move(moveDirection, amount))
      case Move(direction, amount) =>
        copy(location = location.move(direction, amount))
      case Turn(direction, amount) =>
        copy(moveDirection = moveDirection.change(direction, amount))
    }
}
