package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day04.PassportProcessing
import zio.test.Assertion._
import zio.test._

object Day04PassportProcessingSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("PassportProcessingSolutionSpec")(
    testM("PassportProcessing solution") {
      for {
        solution <- PassportProcessing.solution
      } yield assert(solution)(equalTo((230L, 156L)))
    }
  )
}