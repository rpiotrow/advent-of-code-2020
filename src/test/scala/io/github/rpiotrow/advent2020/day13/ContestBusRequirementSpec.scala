package io.github.rpiotrow.advent2020.day13

import cats.data.NonEmptyList
import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment
import zio.test._

object ContestBusRequirementSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day13: ContestBusRequirementSpec")(
      testM("parse 17,x,13,19") {
        val expected = NonEmptyList(
          ContestBusRequirement(17L, 0L),
          List(
            ContestBusRequirement(13L, 2L),
            ContestBusRequirement(19L, 3L)
          )
        )
        for {
          parsed <- ContestBusRequirement.parseList("17,x,13,19")
        } yield assert(parsed)(equalTo(expected))
      },
      testM("parse 67,7,59,61") {
        val expected = NonEmptyList(
          ContestBusRequirement(67L, 0L),
          List(
            ContestBusRequirement(7L, 1L),
            ContestBusRequirement(59L, 2L),
            ContestBusRequirement(61L, 3L)
          )
        )
        for {
          parsed <- ContestBusRequirement.parseList("67,7,59,61")
        } yield assert(parsed)(equalTo(expected))
      }
    )
}
