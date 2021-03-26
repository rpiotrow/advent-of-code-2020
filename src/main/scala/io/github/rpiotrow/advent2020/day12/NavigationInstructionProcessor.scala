package io.github.rpiotrow.advent2020.day12

import zio.ZIO
import zio.blocking.Blocking
import zio.stream.ZStream

object NavigationInstructionProcessor {

  def processInstructions(
    ship: Ship,
    stream: ZStream[Blocking, String, NavigationInstruction]
  ): ZIO[Blocking, String, Ship] =
    stream.fold(ship)((ship, instruction) => ship.navigate(instruction))

}
