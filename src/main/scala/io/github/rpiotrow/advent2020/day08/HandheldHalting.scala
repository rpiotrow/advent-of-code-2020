package io.github.rpiotrow.advent2020.day08

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._

case class ConsoleState(instructionIndex: InstructionIndex, acc: Accumulator)

object ConsoleState {
  val initialState: ConsoleState = ConsoleState(InstructionIndex(0), Accumulator(0))
}

object HandheldHalting {

  val solution: Solution = for {
    program             <- readProgram
    interpreter         =  new ProgramInterpreter(program)
    checker             =  new ProgramVariationsChecker(program)
    beforeLoopState     =  interpreter.interpretUntilLooped
    beforeLoopAcc       =  beforeLoopState.acc.value
    fixedVariationState <- ZIO.fromOption(checker.search).orElseFail("no solution found")
    fixedVariationAcc   =  fixedVariationState.acc.value
    _ <- console.putStrLn(s"Value of the accumulator just before the loop is $beforeLoopAcc")
    _ <- console.putStrLn(s"Value of the accumulator after the program terminates is $fixedVariationAcc")
  } yield (beforeLoopAcc, fixedVariationAcc)

  private def readProgram =
    Input.readLines("day08.input")
      .mapM(Instruction.fromString)
      .runCollect
      .map(c => Program(c.toArray))

}
