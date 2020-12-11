package io.github.rpiotrow.advent2020.day02

import zio.{IO, ZIO}

object Parser {

  def parsePolicyAndPassword(line: String): IO[String, (FirstPasswordPolicy, Password)] =
    for {
      pair                     <- pair(line, ":")
      (policyString, password) =  pair
      policy                   <- parsePolicy(policyString)
    } yield (policy, Password(password))

  private def parsePolicy(string: String): IO[String, FirstPasswordPolicy] =
    for {
      pair                        <- pair(string, " ")
      (rangeString, letterString) =  pair
      range                       <- parseRange(rangeString)
      (min, max)                  =  range
      _                           <- ZIO.succeed(letterString).filterOrDieMessage(_.length == 1)("parse error")
    } yield FirstPasswordPolicy(min, max, letterString(0))

  private def parseRange(rangeString: String): IO[String, (Int, Int)] =
    for {
      pair                   <- pair(rangeString, "-")
      (minString, maxString) =  pair
      min                    <- ZIO.effect(minString.toInt).mapError(_.getMessage)
      max                    <- ZIO.effect(maxString.toInt).mapError(_.getMessage)
    } yield (min, max)

  private def pair(string: String, delimiter: String): IO[String, (String, String)] =
    ZIO.succeed(string.split(delimiter))
      .filterOrDieMessage(_.length == 2)("parse error")
      .map(array => (array(0).trim, array(1).trim))
}
