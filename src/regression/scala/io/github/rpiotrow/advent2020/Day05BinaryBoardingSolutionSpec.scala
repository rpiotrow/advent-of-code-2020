package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day05.BinaryBoarding
import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object Day05BinaryBoardingSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("BinaryBoardingSolutionSpec")(
    testM("BinaryBoarding solution") {
      for {
        solution <- BinaryBoarding.solution
      } yield assert(solution)(equalTo((944L, 554L)))
    }
  )
}
