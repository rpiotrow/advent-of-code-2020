package io.github.rpiotrow.advent2020.day10

import zio.test.Assertion.equalTo
import zio.test._

object AdaptersVariantsSpec extends DefaultRunnableSpec {
  override def spec = suite("day 10: AdaptersVariantsSpec")(
    testM("example adapter variants count") {
      val adapters = Adapters(List(16L, 10L, 15L, 5L, 1L, 11L, 7L, 19L, 6L, 12L, 4L))
      for {
        count <- new AdaptersVariants(adapters).count
      } yield assert(count)(equalTo(8L))
    },
    testM("bigger example adapter variants count") {
      val adapters = Adapters(List(28L, 33L, 18L, 42L, 31L, 14L, 46L, 20L, 48L, 47L, 24L, 23L, 49L,
        45L, 19L, 38L, 39L, 11L, 1L, 32L, 25L, 35L, 8L, 17L, 7L, 9L, 4L, 2L, 34L, 10L, 3L))
      for {
        count <- new AdaptersVariants(adapters).count
      } yield assert(count)(equalTo(19208L))
    }
  )
}
