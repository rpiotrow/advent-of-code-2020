package io.github.rpiotrow.advent2020.day07

import zio.test.Assertion.equalTo
import zio.test._

object BagColorRuleSpec extends DefaultRunnableSpec{
  def spec = suite("day 07: BagColorRuleSpec")(
    testM("parse 'shiny aqua bags contain 1 dark white bag.'") {
      val string = "shiny aqua bags contain 1 dark white bag."
      val expected = BagColorRule(BagColor("shiny aqua"), List((Amount(1), BagColor("dark white"))))
      for {
        result <- BagColorRule.fromString(string)
      } yield assert(result)(equalTo(expected))
    },
    testM("parse 'muted blue bags contain 1 vibrant lavender bag, 4 dotted silver bags, 2 dim indigo bags.'") {
      val string = "muted blue bags contain 1 vibrant lavender bag, 4 dotted silver bags, 2 dim indigo bags."
      val expected = BagColorRule(BagColor("muted blue"), List(
        (Amount(1), BagColor("vibrant lavender")),
        (Amount(4), BagColor("dotted silver")),
        (Amount(2), BagColor("dim indigo"))
      ))
      for {
        result <- BagColorRule.fromString(string)
      } yield assert(result)(equalTo(expected))
    },
    testM("parse 'bright lime bags contain no other bags.'") {
      val string = "bright lime bags contain no other bags."
      val expected = BagColorRule(BagColor("bright lime"), List())
      for {
        result <- BagColorRule.fromString(string)
      } yield assert(result)(equalTo(expected))
    }
  )
}
