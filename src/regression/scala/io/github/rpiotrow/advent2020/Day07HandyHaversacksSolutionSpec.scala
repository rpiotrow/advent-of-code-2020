package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day07.HandyHaversacks
import zio.test.Assertion._
import zio.test._

object Day07HandyHaversacksSolutionSpec extends DefaultRunnableSpec {
  def spec = suite("HandyHaversacksSolutionSpec")(
    testM("HandyHaversacks solution") {
      for {
        solution <- HandyHaversacks.solution
      } yield assert(solution)(equalTo((265L, 14177L)))
    }
  )
}
