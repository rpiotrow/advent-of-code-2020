package io.github.rpiotrow.advent2020.day13

import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object ContestSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 13: ContestSpec")(
      test("17,x,13,19") {
        val requirements: List[ContestBusRequirement] = List(
          ContestBusRequirement(13L, 2L),
          ContestBusRequirement(19L, 3L)
        )
        assert(Contest.compute(17L, requirements))(equalTo(3417L))
      },
      test("67,7,59,61") {
        val requirements: List[ContestBusRequirement] = List(
          ContestBusRequirement(7L, 1L),
          ContestBusRequirement(59L, 2L),
          ContestBusRequirement(61L, 3L)
        )
        assert(Contest.compute(67L, requirements))(equalTo(754018L))
      }
    )
}
