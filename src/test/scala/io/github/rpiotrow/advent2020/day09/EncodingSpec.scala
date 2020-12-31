package io.github.rpiotrow.advent2020.day09

import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object EncodingSpec extends DefaultRunnableSpec {
  private val example = List(
    35L,
    20L,
    15L,
    25L,
    47L,
    40L,
    62L,
    55L,
    65L,
    95L,
    102L,
    117L,
    150L,
    182L,
    127L,
    219L,
    299L,
    277L,
    309L,
    576L)
  def spec = suite("day 09: EncodingSpec")(
    testM("find not matching in example encoding") {
      for {
        first <- new Encoding(example, 5).findFirstNotMatching
      } yield assert(first)(equalTo(127L))
    },
    testM("find contiguous range in example encoding") {
      for {
        range <- new Encoding(example, 5).findContiguousRange(127)
      } yield assert(range)(equalTo((15L, 47L)))
    }
  )

}
