package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day08.HandheldHalting
import zio.test.Assertion._
import zio.test._

object Day08HandheldHaltingSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("HandheldHaltingSolutionSpec")(
    testM("HandheldHalting solution") {
      for {
        solution <- HandheldHalting.solution
      } yield assert(solution)(equalTo((1744L, 1174L)))
    }
  )
}
