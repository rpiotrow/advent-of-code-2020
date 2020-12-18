package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day06.CustomCustoms
import zio.test.Assertion._
import zio.test._

object Day06CustomCustomsSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("CustomCustomsSolutionSpec")(
    testM("CustomCustoms solution") {
      for {
        solution <- CustomCustoms.solution
      } yield assert(solution)(equalTo((6683L, 3122L)))
    }
  )
}
