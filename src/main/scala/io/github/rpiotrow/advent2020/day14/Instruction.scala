package io.github.rpiotrow.advent2020.day14

import zio.{IO, ZIO}

sealed trait Instruction
case class SetMask(mask: Mask) extends Instruction
case class WriteToMemory(address: Int, value: Long) extends Instruction

object Instruction {
  private val maskPattern = """mask = ([X10]+)""".r
  private val memPattern = """mem\[(\d+)\] = (\d+)""".r

  def parse(string: String): IO[String, Instruction] =
    string match {
      case maskPattern(maskStr) => Mask.parse(maskStr).map(SetMask)
      case memPattern(addressStr, valueStr) =>
        for {
          address <- ZIO.effect(addressStr.toInt).mapError(_.getMessage)
          value <- ZIO.effect(valueStr.toLong).mapError(_.getMessage)
        } yield WriteToMemory(address, value)
    }
}
