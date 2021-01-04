package io.github.rpiotrow.advent2020.day09

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.{ZIO, console}

object EncodingError {

  val solution: Solution =
    for {
      list     <- Input.readNumbers("day09.input")
      encoding =  new Encoding(list, 25)
      first    <- encoding.findFirstNotMatching
      range    <- encoding.findContiguousRange(first)
      sum      =  range._1 + range._2
      _ <- console.putStrLn(s"The first number that is not the sum of two of the 25 numbers before it is $first")
      _ <- console.putStrLn(s"Sum of smallest and largest numbers from the contiguous range is $sum")
    } yield (first, sum)

}
