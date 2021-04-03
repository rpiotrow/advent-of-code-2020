package io.github.rpiotrow.advent2020.day13

import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment

object ModularEquationSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 13: ModularEquationSpec")(
      test("67x (mod 7) ≡ 6") {
        assert(ModularEquation(67L, 7L, 6L).x)(equalTo(12L))
      }
    )
}
