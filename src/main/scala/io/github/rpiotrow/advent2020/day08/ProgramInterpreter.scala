package io.github.rpiotrow.advent2020.day08

import scala.annotation.tailrec

class ProgramInterpreter(private val program: Program) {
  def interpretUntilLooped: ConsoleState = {
    @tailrec
    def helper(visited: Set[InstructionIndex], state: ConsoleState): ConsoleState = {
      if (visited.contains(state.instructionIndex))
        state
      else {
        helper(
          visited = visited + state.instructionIndex,
          state = program.instruction(state.instructionIndex).interpret(state)
        )
      }
    }

    helper(Set.empty, ConsoleState(InstructionIndex(0), Accumulator(0)))
  }
}
