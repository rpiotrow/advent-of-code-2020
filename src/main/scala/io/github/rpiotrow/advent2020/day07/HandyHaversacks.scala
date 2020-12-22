package io.github.rpiotrow.advent2020.day07

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.stream.ZStream

case class Amount(value: Int) extends AnyVal
case class BagColor(value: String) extends AnyVal

object HandyHaversacks {

  private val myBag = BagColor("shiny gold")

  val solution: Solution = {
    readRules.broadcast(2, 10).use {
      case streamCopy1::streamCopy2::Nil =>
        for {
          fiber1    <- getReversedRelationsCount(streamCopy1).fork
          fiber2    <- getBagCount(streamCopy2).fork
          count1    <- fiber1.join
          count2    <- fiber2.join
          _         <- console.putStrLn(s"At least one shiny gold bag can eventually contain $count1 bag colors")
          _         <- console.putStrLn(s"In single shiny gold bag $count2 bags are required inside")
        } yield (count1, count2)
      case _ => ZIO.dieMessage("impossible")
    }
  }

  private def getReversedRelationsCount(stream: ZStream[Blocking, String, BagColorRule]): ZIO[Blocking, String, Int] =
    stream
      .map(_.pairsReversed)
      .flatMap(list => ZStream.fromIterable(list))
      .runCollect
      .map(chunks => BagColorReversedRelations.fromReversedPairs(chunks.toList))
      .map(_.eventualBagColors(myBag).size)

  private def getBagCount(stream: ZStream[Blocking, String, BagColorRule]): ZIO[Blocking, String, Long] = {
    for {
      list       <- stream.runCollect.map(_.toList)
      relations  =  BagColorRelations(list)
    } yield relations.countBags(myBag)
  }

  private def readRules: ZStream[Blocking, String, BagColorRule] = {
    Input.readLines("day07.input")
      .mapM(BagColorRule.fromString)
  }
}
