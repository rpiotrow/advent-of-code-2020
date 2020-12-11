package io.github.rpiotrow.advent2020.day02

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.console._

import java.io.IOException

// https://adventofcode.com/2020/day/2
object PasswordPhilosophy {

  val solution: Solution = for {
    lines              <- readLines
    firstPolicyEntries <- parse(lines)
    _                  <- putStrLn(s"There are ${countValid(firstPolicyEntries)} valid passwords for the first policy.")
    secondPolicyEntries = firstPolicyEntries.map({ case (policy, password) => (policy.toSecondPolicy, password)} )
    _                  <- putStrLn(s"There are ${countValid(secondPolicyEntries)} valid passwords for the second policy.")
  } yield ()

  private def readLines: ZIO[Blocking, IOException, List[String]] =
    Input.readLines("day02.input")
      .runCollect
      .map(_.toList)

  private def parse(list: List[String]): Task[List[(FirstPasswordPolicy, Password)]] =
    ZIO.collectAll(list.map(Parser.parsePolicyAndPassword)).mapError(new RuntimeException(_))

  private def countValid(list: List[(PasswordPolicy, Password)]): Int =
    list.count { case (policy, password) => policy.isPasswordValid(password) }

}
