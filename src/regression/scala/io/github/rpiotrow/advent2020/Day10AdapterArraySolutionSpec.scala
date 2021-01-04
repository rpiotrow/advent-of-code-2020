package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day10.AdapterArray
import zio.test.Assertion._
import zio.test._

object Day10AdapterArraySolutionSpec extends DefaultRunnableSpec {
  def spec = suite("AdapterArraySolutionSpec")(
    testM("AdapterArray solution") {
      for {
        solution <- AdapterArray.solution
      } yield assert(solution)(equalTo((2432L, 453551299002368L)))
    }
  )
}
