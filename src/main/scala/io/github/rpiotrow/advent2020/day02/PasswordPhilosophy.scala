package io.github.rpiotrow.advent2020.day02

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._
import zio.blocking.Blocking
import zio.console._
import zio.stream.ZStream

// https://adventofcode.com/2020/day/2
object PasswordPhilosophy {

  val solution: Solution =
    readRows.broadcast(2, 10).use {
      case streamCopy1::streamCopy2::Nil =>
        for {
          fiberFirst         <- countValid(streamCopy1).fork
          secondPolicyStream =  streamCopy2.map({ case (policy, password) => (policy.toSecondPolicy, password)} )
          fiberSecond        <- countValid(secondPolicyStream).fork
          countValidFirst    <- fiberFirst.join
          countValidSecond   <- fiberSecond.join
          _                  <- putStrLn(s"There are $countValidFirst valid passwords for the first policy.")
          _                  <- putStrLn(s"There are $countValidSecond valid passwords for the second policy.")
        } yield (countValidFirst, countValidSecond)
      case _ => ZIO.dieMessage("impossible")
    }

  private def readRows: ZStream[Blocking, String, (FirstPasswordPolicy, Password)] =
    Input.readLines("day02.input")
      .mapM(Parser.parsePolicyAndPassword)

  private def countValid(stream: ZStream[Blocking, String, (PasswordPolicy, Password)]) =
    stream.filter { case (policy, password) => policy.isPasswordValid(password) }.runCount

}
