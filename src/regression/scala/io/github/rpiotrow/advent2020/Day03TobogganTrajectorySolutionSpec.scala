package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day03.TobogganTrajectory
import zio.test.Assertion._
import zio.test._

object Day03TobogganTrajectorySolutionSpec extends DefaultRunnableSpec {
  def spec = suite("TobogganTrajectorySolutionSpec")(
    testM("TobogganTrajectory solution") {
      for {
        solution <- TobogganTrajectory.solution
      } yield assert(solution)(equalTo((203L, 3316272960L)))
    }
  )
}