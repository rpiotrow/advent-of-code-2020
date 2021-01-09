package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day11.SeatingSystem
import zio.test.Assertion._
import zio.test._

object Day11SeatingSystemSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("SeatingSystemSolutionSpec")(
    testM("SeatingSystem solution") {
      for {
        solution <- SeatingSystem.solution
      } yield assert(solution)(equalTo((2359L, 0L)))
    }
  )
}
