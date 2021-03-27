package io.github.rpiotrow.advent2020.day13

import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment

object BusShuttleSpec extends DefaultRunnableSpec {
  private val exampleInput = List(
    "939",
    "7,13,x,x,59,x,31,19"
  )
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Day 13: BusShuttleSpec")(
      testM("parse example input") {
        for {
          parsed <- BusShuttle.parse(exampleInput)
          expectedBuses = List(Bus(7L), Bus(13L), Bus(59L), Bus(31L), Bus(19L))
        } yield assert(parsed)(equalTo(BusShuttle(Timestamp(939L), expectedBuses)))
      },
      testM("earliest departure for example input") {
        for {
          parsed <- BusShuttle.parse(exampleInput)
          departure = parsed.earliestDeparture
        } yield assert(departure)(equalTo(BusDeparture(Bus(59L), 5L)))
      }
    )
}
