package io.github.rpiotrow.advent2020.day14

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._

object DockingData {
  val solution: Solution = {
    for {
      computerSystem <- Input
        .readLines("day14.input")
        .mapM(Instruction.parse)
        .fold(ComputerSystem.zero)((system, instruction) => system.process(instruction))
      sum = computerSystem.sum
      _ <- console.putStrLn(s"The sum of all values left in memory is $sum")
    } yield (sum, -1L)
  }
}
