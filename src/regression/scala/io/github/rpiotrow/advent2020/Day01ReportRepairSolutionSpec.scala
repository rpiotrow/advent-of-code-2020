package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day01.ReportRepair
import zio.test.Assertion._
import zio.test._

object Day01ReportRepairSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("ReportRepairSolutionSpec")(
    testM("ReportRepair solution") {
      for {
        solution <- ReportRepair.solution
      } yield assert(solution)(equalTo((987339L, 259521570L)))
    }
  )
}