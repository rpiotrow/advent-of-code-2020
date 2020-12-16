package io.github.rpiotrow.advent2020.day01

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.console._

// https://adventofcode.com/2020/day/1
object ReportRepair {

  private val SUM = 2020

  val solution: Solution = for {
      set           <- readReportEntries
      twoEntries    <- ZIO.fromOption(findTwoEntries(set)).orElseFail("solution not found")
      multiplyTwo   =  multiply(twoEntries)
      threeEntries  <- ZIO.fromOption(findThreeEntries(makeProduct(set.toList), set)).orElseFail("solution not found")
      multiplyThree =  multiply(threeEntries)
      _             <- putStrLn(s"Answer to part one is: $multiplyTwo")
      _             <- putStrLn(s"Answer to part two is: $multiplyThree")
    } yield (multiplyTwo, multiplyThree)

  private def readReportEntries: ZIO[Blocking, String, Set[Int]] =
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
