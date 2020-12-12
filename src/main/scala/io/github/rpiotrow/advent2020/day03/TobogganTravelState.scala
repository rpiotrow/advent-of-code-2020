package io.github.rpiotrow.advent2020.day03

case class TobogganTravelState(x: Int = 0, treesEncountered: Int = 0) {
  def down(row: TreeRow, right: Int): TobogganTravelState = {
    val newPosition = this.x + right
    TobogganTravelState(
      x = newPosition,
      treesEncountered = this.treesEncountered + (if (row.hasTreeAt(newPosition)) 1 else 0)
    )
  }
}
