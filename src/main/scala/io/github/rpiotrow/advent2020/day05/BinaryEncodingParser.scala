package io.github.rpiotrow.advent2020.day05

import zio.{IO, ZIO}

class BinaryEncodingParser(zero: Char, one: Char) {
  def parse(string: String): IO[String, Int] =
    ZIO.foldLeft(string)(0)((acc, char) =>
      if (char == zero)
        ZIO.succeed(acc * 2)
      else if (char == one)
        ZIO.succeed((acc * 2) + 1)
      else
        ZIO.fail[String]("parse error")
    )
}
