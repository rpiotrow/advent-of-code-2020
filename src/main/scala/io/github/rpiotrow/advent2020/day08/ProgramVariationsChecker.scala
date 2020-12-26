package io.github.rpiotrow.advent2020.day08

import cats.implicits._

class ProgramVariationsChecker(private val program: Program) {

  def search: Option[ConsoleState] =
    search(Set.empty, ConsoleState.initialState, instructionChanged = false)

  private def search(
      visited: Set[InstructionIndex],
      consoleState: ConsoleState,
      instructionChanged: Boolean): Option[ConsoleState] =
    if (visited.contains(consoleState.instructionIndex))
      None
    else if (program.isTerminated(consoleState))
      consoleState.some
    else
      checkAllPossibilities(visited, consoleState, instructionChanged)

  private def checkAllPossibilities(
      visited: Set[InstructionIndex],
      consoleState: ConsoleState,
      instructionChanged: Boolean) = {
    program.instruction(consoleState.instructionIndex) match {
      case instruction @ AccChange(_) =>
        move(visited, consoleState, instructionChanged, instruction)
      case instruction @ Nop(amount) =>
        move(visited, consoleState, instructionChanged, instruction) orElse {
          if (instructionChanged)
            None
          else
            move(visited, consoleState, instructionChanged = true, Jump(amount))
        }
      case instruction @ Jump(amount) =>
        move(visited, consoleState, instructionChanged, instruction) orElse {
          if (instructionChanged)
            None
          else
            move(visited, consoleState, instructionChanged = true, Nop(amount))
        }
    }
  }

  private def move(
    visited: Set[InstructionIndex],
    consoleState: ConsoleState,
    instructionChanged: Boolean,
    instruction: Instruction) =
    search(
      visited + consoleState.instructionIndex,
      instruction.interpret(consoleState),
      instructionChanged
    )
}
