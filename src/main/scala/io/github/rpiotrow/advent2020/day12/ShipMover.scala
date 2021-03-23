package io.github.rpiotrow.advent2020.day12

object ShipMover {

  def move(shipState: Ship, instructions: List[NavigationInstruction]): Ship =
    instructions.foldLeft(shipState)((ship, instruction) => ship.navigate(instruction))

}
