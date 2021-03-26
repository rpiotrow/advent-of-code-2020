package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day12.RainRisk
import zio.test.Assertion._
import zio.test._

object Day12RainRiskSolutionSpec extends DefaultRunnableSpec {
  def spec =
    suite("RainRiskSolutionSpec")(
      testM("RainRisk solution") {
        for {
          solution <- RainRisk.solution
        } yield assert(solution)(equalTo((845L, 27016L)))
      }
    )
}
