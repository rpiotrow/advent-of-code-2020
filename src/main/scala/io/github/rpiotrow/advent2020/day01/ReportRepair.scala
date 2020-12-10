package io.github.rpiotrow.advent2020.day01

import io.github.rpiotrow.advent2020.Input
import zio._
import zio.blocking.Blocking
import zio.console._

import java.io.IOException

// https://adventofcode.com/2020/day/1
object ReportRepair extends zio.App {

  private val SUM = 2020

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    myAppLogic.exitCode

  val myAppLogic: ZIO[Console with Blocking, Serializable, Unit] = for {
      set          <- readReportEntries
      twoEntries   <- ZIO.fromOption(findTwoEntries(set))
      _            <- putStrLn(s"Answer to part one is: ${multiply(twoEntries)}")
      threeEntries <- ZIO.fromOption(findThreeEntries(makeProduct(set.toList), set))
      _            <- putStrLn(s"Answer to part two is: ${multiply(threeEntries)}")
    } yield ()

  private def readReportEntries: ZIO[Blocking, IOException, Set[Int]] =
    Input.readLines("day01.input")
        .map(_.toInt)
        .runCollect
        .map(_.toSet)

  private def makeProduct(elements: List[Int]): List[(Int, Int)] =
    for {
      x <- elements
      y <- elements
    } yield (x, y)

  private def findTwoEntries(set: Set[Int]): Option[(Int, Int)] = {
    set
      .find(entry => set.contains(SUM - entry))
      .map(first => (first, SUM - first))
  }

  private def findThreeEntries(list: List[(Int, Int)], set: Set[Int]): Option[(Int, Int, Int)] = {
    list
      .find({ case (e1, e2) => set.contains(SUM - (e1 + e2)) })
      .map({ case (e1, e2) => (e1, e2, SUM - (e1 + e2)) })
  }

  private def multiply(pair: (Int, Int)): Int =
    pair match {
      case (e1, e2) => e1*e2
    }

  private def multiply(triple: (Int, Int, Int)): Int =
    triple match {
      case (e1, e2, e3) => e1*e2*e3
    }
}
