package io.github.rpiotrow.advent2020.day08

import zio.test.Assertion.equalTo
import zio.test._

object ProgramInterpreterSpec extends DefaultRunnableSpec {
  def spec = suite("day 08: ProgramInterpreterSpec")(
    test("interpret example program") {
      val program = Program(Array(
        Nop(0),
        AccChange(1),
        Jump(4),
        AccChange(3),
        Jump(-3),
        AccChange(-99),
        AccChange(1),
        Jump(-4),
        AccChange(6)
      ))
      val interpreter = new ProgramInterpreter(program)
      val expectedFinalState = ConsoleState(InstructionIndex(1), Accumulator(5))
      assert(interpreter.interpretUntilLooped)(equalTo(expectedFinalState))
    }
  )
}
