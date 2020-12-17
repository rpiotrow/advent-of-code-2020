package io.github.rpiotrow.advent2020.day05

import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object BinaryEncodingParserSpec extends DefaultRunnableSpec {
  def spec = suite("day 05: BinaryEncodingParserSpec")(
    testM("parse FBFBBFF") {
      val parser = new BinaryEncodingParser('F', 'B')
      for {
        result <- parser.parse("FBFBBFF")
      } yield assert(result)(equalTo(44))
    },
    testM("parse RLR") {
      val parser = new BinaryEncodingParser('L', 'R')
      for {
        result <- parser.parse("RLR")
      } yield assert(result)(equalTo(5))
    }
  )
}

