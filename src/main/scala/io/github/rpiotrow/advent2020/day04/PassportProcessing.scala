package io.github.rpiotrow.advent2020.day04

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio.ZIO
import zio.blocking.Blocking
import zio.console.putStrLn
import zio.stream.{ZStream, ZTransducer}

object PassportProcessing {

  val solution: Solution = {
    readPassports
      .map(_.requiredFields)
      .filter(_.isDefined)
      .broadcast(2, 10).use {
        case stream1::stream2::Nil => for {
          part1 <- countPresent(stream1).fork
          part2 <- countValid(stream2).fork
          presentCount <- part1.join
          validCount <- part2.join
          _ <- putStrLn(s"There are $presentCount passports with required fields present.")
          _ <- putStrLn(s"There are $validCount passports with required fields present and valid.")
        } yield ()
        case _ => ZIO.dieMessage("cannot happen")
      }
  }

  private def countPresent(stream: ZStream[Any, String, Option[PassportRequiredFields]]) =
    stream.runCount

  private def countValid(stream: ZStream[Any, String, Option[PassportRequiredFields]]) =
    stream
      .map(_.get)
      .filter(_.areValid)
      .runCount

  private def readPassports: ZStream[Blocking, String, PassportData] =
    Input.readStrings("day04.input")
      .transduce(ZTransducer.splitOn("\n\n"))
      .mapM(Parser.parsePassport)

}
