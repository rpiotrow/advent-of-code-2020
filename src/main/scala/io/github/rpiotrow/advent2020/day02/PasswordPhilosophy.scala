package io.github.rpiotrow.advent2020.day02

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.console._

// https://adventofcode.com/2020/day/2
object PasswordPhilosophy {

  val solution: Solution = for {
    lines               <- readLines
    firstPolicyEntries  <- parse(lines)
    countValidFirst     =  countValid(firstPolicyEntries)
    _                   <- putStrLn(s"There are $countValidFirst valid passwords for the first policy.")
    secondPolicyEntries = firstPolicyEntries.map({ case (policy, password) => (policy.toSecondPolicy, password)} )
    countValidSecond    = countValid(secondPolicyEntries)
    _                   <- putStrLn(s"There are $countValidSecond valid passwords for the second policy.")
  } yield (countValidFirst, countValidSecond)

  private def readLines: ZIO[Blocking, String, List[String]] =
    Input.readLines("day02.input")
      .runCollect
      .map(_.toList)

  private def parse(list: List[String]): IO[String, List[(FirstPasswordPolicy, Password)]] =
    ZIO.collectAll(list.map(Parser.parsePolicyAndPassword))

  private def countValid(list: List[(PasswordPolicy, Password)]): Int =
    list.count { case (policy, password) => policy.isPasswordValid(password) }

}
