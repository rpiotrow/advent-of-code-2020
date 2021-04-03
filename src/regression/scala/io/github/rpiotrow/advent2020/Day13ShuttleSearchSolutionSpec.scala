package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day13.ShuttleSearch
import zio.test.Assertion._
import zio.test._

object Day13ShuttleSearchSolutionSpec extends DefaultRunnableSpec {
  def spec =
    suite("ShuttleSearchSolutionSpec")(
      testM("ShuttleSearch solution") {
        for {
          solution <- ShuttleSearch.solution
        } yield assert(solution)(equalTo((6559L, 626670513163231L)))
      }
    )
}
