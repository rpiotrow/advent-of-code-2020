package io.github.rpiotrow.advent2020.day02

import zio.test.Assertion._
import zio.test._

object ParserSpec extends DefaultRunnableSpec {
  def spec = suite("HelloWorldSpec")(
    testM("parse \"1-3 a: abcde\"") {
      for {
        parsed <- Parser.parsePolicyAndPassword("1-3 a: abcde")
      } yield assert(parsed)(equalTo((FirstPasswordPolicy(1, 3, 'a'), Password("abcde"))))
    },
    testM("parse \"1-3 b: cdefg\"") {
      for{
        parsed <- Parser.parsePolicyAndPassword("1-3 b: cdefg")
      } yield assert(parsed)(equalTo((FirstPasswordPolicy(1, 3, 'b'), Password("cdefg"))))
    },
    testM("parse \"2-9 c: ccccccccc\"") {
      for {
        parsed <- Parser.parsePolicyAndPassword("2-9 c: ccccccccc")
      } yield assert(parsed)(equalTo((FirstPasswordPolicy(2, 9, 'c'), Password("ccccccccc"))))
    }
  )
}