package io.github.rpiotrow.advent2020

import io.github.rpiotrow.advent2020.day02.PasswordPhilosophy
import zio.test.Assertion._
import zio.test._

object Day02PasswordPhilosophySolutionSpec extends DefaultRunnableSpec {
  def spec = suite("PasswordPhilosophySolutionSpec")(
    testM("PasswordPhilosophy solution") {
      for {
        solution <- PasswordPhilosophy.solution
      } yield assert(solution)(equalTo((564L, 325L)))
    }
  )
}