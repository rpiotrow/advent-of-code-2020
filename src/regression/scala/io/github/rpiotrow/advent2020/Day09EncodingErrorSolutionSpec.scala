package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day09.EncodingError
import zio.test.Assertion._
import zio.test._

object Day09EncodingErrorSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("EncodingErrorSolutionSpec")(
    testM("EncodingError solution") {
      for {
        solution <- EncodingError.solution
      } yield assert(solution)(equalTo((1124361034L, 129444555L)))
    }
  )
}
