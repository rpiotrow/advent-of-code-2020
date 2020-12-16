package io.github.rpiotrow.advent2020.day03

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.putStrLn
import zio.stream.{Sink, ZStream}

// https://adventofcode.com/2020/day/3
object TobogganTrajectory {

  val solution: Solution =
    treeRowStream.broadcast(5, 10).use {
      case streamCopy1::streamCopy2::streamCopy3::streamCopy4::streamCopy5::Nil =>
        for {
          fiber11        <- travel(1, streamCopy1).fork
          fiber31        <- travel(3, streamCopy2).fork
          fiber51        <- travel(5, streamCopy3).fork
          fiber71        <- travel(7, streamCopy4).fork
          fiber12        <- travel(1, omit1(streamCopy5)).fork
          slope11        <- fiber11.join
          slope31        <- fiber31.join
          slope51        <- fiber51.join
          slope71        <- fiber71.join
          slope12        <- fiber12.join
          list           =  List(slope11, slope31, slope51, slope71, slope12)
          multiplication =  list.map(_.treesEncountered.toLong).product
          _              <- putStrLn(s"You would encounter ${slope31.treesEncountered} trees.")
          _              <- putStrLn(s"Multiplication of number of trees encountered on each of the slopes is $multiplication.")
        } yield (slope31.treesEncountered, multiplication)
      case _ => ZIO.dieMessage("impossible")
    }

  private def travel[R](right: Int, stream: ZStream[R, String, TreeRow]): ZIO[R, String, TobogganTravelState] =
    stream.run(Sink.foldLeft(TobogganTravelState(-right))((state, row) => state.down(row, right)))

  private def omit1(stream: ZStream[Blocking, String, TreeRow]): ZStream[Blocking with Clock, String, TreeRow] = {
    val infiniteTrueFalse: ZStream[Clock, Nothing, Boolean] =
      ZStream.fromIterable(List(ZStream.succeed(true), ZStream.succeed(false))).flatten.repeat(Schedule.forever)
    stream.zip(infiniteTrueFalse).filter({ case (_, filter) => filter}).map({ case (row, _) => row})
  }

  private def treeRowStream: ZStream[Blocking, String, TreeRow] = {
    Input.readLines("day03.input")
      .mapM(TreeRow.fromString)
  }
}
