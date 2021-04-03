package io.github.rpiotrow.advent2020.day14

import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment
import zio.test._

object InstructionSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 14: InstructionSpec")(
      testM("parse mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") {
        for {
          instruction <- Instruction.parse("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
        } yield assert(instruction)(equalTo(SetMask(Mask(64L, ~2L))))
      },
      testM("parse mem[8] = 11") {
        for {
          instruction <- Instruction.parse("mem[8] = 11")
        } yield assert(instruction)(equalTo(WriteToMemory(8, 11L)))
      }
    )
}
