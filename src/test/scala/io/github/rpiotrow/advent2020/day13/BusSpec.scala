package io.github.rpiotrow.advent2020.day13

import zio.test._
import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment

object BusSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 13: BusSpec")(
      test("bus 59 earliest departure in minutes for 939") {
        assert(Bus(59L).earliestDepartureInMinutes(Timestamp(939L)))(equalTo(5L))
      },
      test("bus 59 earliest departure in minutes for 944") {
        assert(Bus(59L).earliestDepartureInMinutes(Timestamp(944L)))(equalTo(0L))
      }
    )
}
