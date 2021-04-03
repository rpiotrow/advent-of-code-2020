package io.github.rpiotrow.advent2020.day14

import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment
import zio.test._

object MaskSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 14: MaskSpec")(
      testM("parse XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") {
        for {
          mask <- Mask.parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
        } yield assert(mask)(equalTo(Mask(64L, ~2L)))
      },
      test("apply XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X to 11") {
        assert(Mask(64L, ~2L).apply(11L))(equalTo(73L))
      },
      test("apply XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X to 101") {
        assert(Mask(64L, ~2L).apply(101L))(equalTo(101L))
      }
    )
}
