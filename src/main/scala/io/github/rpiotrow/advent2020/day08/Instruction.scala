package io.github.rpiotrow.advent2020.day08

import zio._

sealed trait Instruction {
  def interpret(state: ConsoleState): ConsoleState
}
case class AccChange(amount: Int) extends Instruction {
  def interpret(state: ConsoleState): ConsoleState =
    ConsoleState(state.instructionIndex.next, state.acc.change(amount))
}
case class Jump(delta: Int) extends Instruction {
  def interpret(state: ConsoleState): ConsoleState =
    ConsoleState(state.instructionIndex.change(delta), state.acc)
}
case class Nop(number: Int) extends Instruction {
  def interpret(state: ConsoleState): ConsoleState =
    ConsoleState(state.instructionIndex.next, state.acc)
}

object Instruction {
  private val accPattern = raw"acc ([+-]\d+)".r
  private val nopPattern = raw"nop ([+-]\d+)".r
  private val jmpPattern = raw"jmp ([+-]\d+)".r

  def fromString(string: String): IO[String, Instruction] = {
    string match {
      case accPattern(valueString) => ZIO.succeed(AccChange(valueString.toInt))
      case jmpPattern(valueString) => ZIO.succeed(Jump(valueString.toInt))
      case nopPattern(valueString) => ZIO.succeed(Nop(valueString.toInt))
      case _ => ZIO.fail(s"cannot parse instruction $string")
    }
  }
}
