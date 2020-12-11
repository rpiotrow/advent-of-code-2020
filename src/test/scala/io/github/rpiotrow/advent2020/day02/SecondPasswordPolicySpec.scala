package io.github.rpiotrow.advent2020.day02

import zio.test.Assertion._
import zio.test._

object SecondPasswordPolicySpec extends DefaultRunnableSpec {
  def spec = suite("HelloWorldSpec")(
    test("policy 1-3 a") {
      assert(SecondPasswordPolicy(1, 3, 'a').isPasswordValid(Password("abcde")))(equalTo(true))
    },
    test("policy 1-3 b") {
      assert(SecondPasswordPolicy(1, 3, 'b').isPasswordValid(Password("cdefg")))(equalTo(false))
    },
    test("policy 2-9 c") {
      assert(SecondPasswordPolicy(2, 9, 'c').isPasswordValid(Password("ccccccccc")))(equalTo(false))
    }
  )
}