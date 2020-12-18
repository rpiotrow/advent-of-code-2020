package io.github.rpiotrow.advent2020.day06

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.stream._

case class CustomsDeclarationFormData(lines: String) extends AnyVal

// https://adventofcode.com/2020/day/6
object CustomCustoms {

  val solution: Solution =
    readData.broadcast(2, 10).use({
      case streamCopy1::streamCopy2::Nil =>
        for {
          fiber1 <- sumCounts(streamCopy1, data => CustomsDeclarationForm.fromStringWhenAny(data.lines)).fork
          fiber2 <- sumCounts(streamCopy2, data => CustomsDeclarationForm.fromStringWhenAll(data.lines)).fork
          sumAny <- fiber1.join
          sumAll <- fiber2.join
          _      <- console.putStrLn(s"The sum of the counts if any question is yes: $sumAny")
          _      <- console.putStrLn(s"The sum of the counts if all question is yes: $sumAll")
        } yield (sumAny, sumAll)
      case _ => ZIO.dieMessage("impossible")
    })

  private def readData: ZStream[Blocking, String, CustomsDeclarationFormData] =
    Input.readStrings("day06.input")
      .transduce(ZTransducer.splitOn("\n\n"))
      .map(CustomsDeclarationFormData)

  private def sumCounts[E](
    stream: ZStream[Blocking, String, CustomsDeclarationFormData],
    parser: CustomsDeclarationFormData => IO[String, CustomsDeclarationForm]
  ): ZIO[Blocking, String, Int] = {
    stream
      .mapM(parser)
      .map(_.yesAnswers.size)
      .runSum
  }
}
