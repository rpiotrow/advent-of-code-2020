package io.github.rpiotrow.advent2020.day12

import zio.test.Assertion.equalTo
import zio.test._

object NavigationInstructionSpec extends DefaultRunnableSpec {
  override def spec = suite("day 12: NavigationInstructionSpec")(
    testM("parse F10") {
      for {
        instruction <- NavigationInstruction.parse("F10")
      } yield assert(instruction)(equalTo(MoveForward(10)))
    },
    testM("parse N3") {
      for {
        instruction <- NavigationInstruction.parse("N3")
      } yield assert(instruction)(equalTo(Move(North, 3)))
    },
    testM("parse F7") {
      for {
        instruction <- NavigationInstruction.parse("F7")
      } yield assert(instruction)(equalTo(MoveForward(7)))
    },
    testM("parse R90") {
      for {
        instruction <- NavigationInstruction.parse("R90")
      } yield assert(instruction)(equalTo(Turn(RightDirection, Degrees90)))
    },
    testM("parse R270") {
      for {
        instruction <- NavigationInstruction.parse("R270")
      } yield assert(instruction)(equalTo(Turn(RightDirection, Degrees270)))
    },
    testM("parse L180") {
      for {
        instruction <- NavigationInstruction.parse("L180")
      } yield assert(instruction)(equalTo(Turn(LeftDirection, Degrees180)))
    },
    testM("parse F11") {
      for {
        instruction <- NavigationInstruction.parse("F11")
      } yield assert(instruction)(equalTo(MoveForward(11)))
    }
  )
}
