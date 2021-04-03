package io.github.rpiotrow.advent2020.day13

import zio.test._
import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment

object ExtendedGCDSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 13: ExtendedGCDSpec")(
      test("ExtendedGCDSpec(17,19)") {
        val extendedGCD = ExtendedGCD(17L, 19L)
        assert(extendedGCD.gcd)(equalTo(1L)) &&
          assert(extendedGCD.x)(equalTo(9L)) &&
          assert(extendedGCD.y)(equalTo(-8L))
      }
    )
}
