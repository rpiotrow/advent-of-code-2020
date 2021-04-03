package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day14.DockingData
import zio.test.Assertion._
import zio.test._

object Day14DockingDataSolutionSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Any] =
    suite("DockingDataSolutionSpec")(
      testM("DockingData solution") {
        for {
          solution <- DockingData.solution
        } yield assert(solution)(equalTo((16003257187056L, -1L)))
      }
    )
}
