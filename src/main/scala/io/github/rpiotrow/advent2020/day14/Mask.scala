package io.github.rpiotrow.advent2020.day14

import zio.IO

case class Mask(onesMask: Long, notZerosMask: Long) {
  def apply(value: Long): Long =
    (value | onesMask) & notZerosMask
}

object Mask {
  private case class ParseAcc(bit: Long, oneMask: Long, zeroMask: Long)

  def parse(string: String): IO[String, Mask] =
    IO.foldRight(string.toList)(ParseAcc(1L, 0L, 0L)) { (c, acc) =>
      val newBit = acc.bit*2
      c match {
        case 'X' => IO.succeed(ParseAcc(newBit, acc.oneMask, acc.zeroMask))
        case '1' => IO.succeed(ParseAcc(newBit, acc.oneMask+acc.bit, acc.zeroMask))
        case '0' => IO.succeed(ParseAcc(newBit, acc.oneMask, acc.zeroMask+acc.bit))
        case _ => IO.fail("invalid input")
      }
    }.map(acc => Mask(acc.oneMask, ~acc.zeroMask))
}
