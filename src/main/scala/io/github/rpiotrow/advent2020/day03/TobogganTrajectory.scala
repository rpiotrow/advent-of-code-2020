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
    for {
      slope31        <- travel(3, stream1)
      _              <- putStrLn(s"You would encounter ${slope31.treesEncountered} trees.")
      slope11        <- travel(1, stream1)
      slope51        <- travel(5, stream1)
      slope71        <- travel(7, stream1)
      slope12        <- travel(1, stream2)
      list           =  List(slope11, slope31, slope51, slope71, slope12)
      multiplication =  list.map(_.treesEncountered.toLong).product
      _              <- putStrLn(s"Multiplication of number of trees encountered on each of the slopes is $multiplication.")
    } yield ()

  private def travel[R](right: Int, stream: ZStream[R, String, TreeRow]): ZIO[R, String, TobogganTravelState] =
    stream.run(Sink.foldLeft(TobogganTravelState(-right))((state, row) => state.down(row, right)))

  private def stream1: ZStream[Blocking, String, TreeRow] = treeRowStream
  private def stream2: ZStream[Blocking with Clock, String, TreeRow] = {
    val infiniteTrueFalse: ZStream[Clock, Nothing, Boolean] =
      ZStream.fromIterable(List(ZStream.succeed(true), ZStream.succeed(false))).flatten.repeat(Schedule.forever)
    stream1.zip(infiniteTrueFalse).filter({ case (_, filter) => filter}).map({ case (row, _) => row})
  }

  private def treeRowStream: ZStream[Blocking, String, TreeRow] = {
    Input.readLines("day03.input")
      .mapError(_.getMessage)
      .mapM(TreeRow.fromString)
  }
}
