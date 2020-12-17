package io.github.rpiotrow.advent2020.day05

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking

case class BoardingPassSeat(row: Int, column: Int) {
  def seatId: Int = row * 8 + column
}

// https://adventofcode.com/2020/day/5
object BinaryBoarding {

  val solution: Solution = {
    for {
      seatIdList <- readBoardingPassSeatIds
      seatIdSet  =  seatIdList.toSet
      min        =  seatIdList.head
      max        =  seatIdList.last
      seatId     <- ZIO.fromOption(List.range(min, max).find(isFree(seatIdSet))).orElseFail("seat not found")
      _          <- console.putStrLn(s"The highest seat ID is $max")
      _          <- console.putStrLn(s"My seat id is $seatId")
    } yield (max, seatId)
  }

  private def isFree(seatIdSet: Set[Int])(seatId: Int) =
    !seatIdSet.contains(seatId) &&
      seatIdSet.contains(seatId-1) &&
      seatIdSet.contains(seatId+1)

  private def readBoardingPassSeatIds: ZIO[Blocking, String, List[Int]] = {
    val rowColumnPattern = raw"([FB]{7})([LR]{3})".r
    val rowParser = new BinaryEncodingParser('F', 'B')
    val columnParser = new BinaryEncodingParser('L', 'R')
    Input.readLines("day05.input")
      .mapM({
        case rowColumnPattern(rowEncoded, columnEncoded) =>
          for {
            row    <- rowParser.parse(rowEncoded)
            column <- columnParser.parse(columnEncoded)
          } yield BoardingPassSeat(row, column)
        case e => ZIO.fail(s"invalid input: '$e'")
      })
      .map(_.seatId)
      .runCollect
      .map(_.toList.sorted)
  }
}
