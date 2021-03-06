package io.github.rpiotrow.advent2020.day02

import zio.test._
import zio.test.Assertion._

object FirstPasswordPolicySpec extends DefaultRunnableSpec {
  def spec = suite("day02: FirstPasswordPolicySpec")(
    test("first password policy 1-3 a") {
      assert(FirstPasswordPolicy(1, 3, 'a').isPasswordValid(Password("abcde")))(equalTo(true))
    },
    test("first password policy 1-3 b") {
      assert(FirstPasswordPolicy(1, 3, 'b').isPasswordValid(Password("cdefg")))(equalTo(false))
    },
    test("first password policy 2-9 c") {
      assert(FirstPasswordPolicy(2, 9, 'c').isPasswordValid(Password("ccccccccc")))(equalTo(true))
    }
  )
}