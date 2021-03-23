package io.github.rpiotrow.advent2020.day12

import zio._

sealed trait NavigationInstruction

case class MoveForward(amount: Int) extends NavigationInstruction
case class Move(direction: MoveDirection, amount: Int) extends NavigationInstruction
case class Turn(direction: TurnDirection, amount: TurnAmount) extends NavigationInstruction

sealed trait TurnDirection
case object LeftDirection extends TurnDirection
case object RightDirection extends TurnDirection

sealed trait TurnAmount
case object Degrees90 extends TurnAmount
case object Degrees180 extends TurnAmount
case object Degrees270 extends TurnAmount

object NavigationInstruction {
  private val northPattern = raw"N(\d+)".r
  private val southPattern = raw"S(\d+)".r
  private val eastPattern = raw"E(\d+)".r
  private val westPattern = raw"W(\d+)".r
  private val leftPattern = raw"L(\d+)".r
  private val rightPattern = raw"R(\d+)".r
  private val forwardPattern = raw"F(\d+)".r

  def parse(string: String): IO[String, NavigationInstruction] =
    string match {
      case northPattern(amountStr) =>
        parseIntAmount(amountStr) map (Move(North, _))
      case southPattern(amountStr) =>
        parseIntAmount(amountStr) map (Move(South, _))
      case eastPattern(amountStr) =>
        parseIntAmount(amountStr) map (Move(East, _))
      case westPattern(amountStr) =>
        parseIntAmount(amountStr) map (Move(West, _))
      case forwardPattern(amountStr) =>
        parseIntAmount(amountStr) map MoveForward
      case leftPattern("90") =>
        IO.succeed(Turn(LeftDirection, Degrees90))
      case leftPattern("180") =>
        IO.succeed(Turn(LeftDirection, Degrees180))
      case leftPattern("270") =>
        IO.succeed(Turn(LeftDirection, Degrees270))
      case rightPattern("90") =>
        IO.succeed(Turn(RightDirection, Degrees90))
      case rightPattern("180") =>
        IO.succeed(Turn(RightDirection, Degrees180))
      case rightPattern("270") =>
        IO.succeed(Turn(RightDirection, Degrees270))
      case unknown =>
        IO.fail(s"unknown instruction: '$unknown'")
    }

  private def parseIntAmount(string: String): IO[String, Int] =
    ZIO.effect(string.toInt).mapError(_.getMessage)
}

