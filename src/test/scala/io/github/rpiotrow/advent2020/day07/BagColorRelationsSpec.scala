package io.github.rpiotrow.advent2020.day07

import zio.test.Assertion.equalTo
import zio.test._

object BagColorRelationsSpec extends DefaultRunnableSpec {
  def spec = suite("day 07: BagColorRelationsSpec")(
    test("bag count for shiny gold bag in example 1") {
      val count = BagColorRelations(
        List(
          BagColorRule(BagColor("light red"), List((Amount(1), BagColor("bright white")), (Amount(2), BagColor("muted yellow")))),
          BagColorRule(BagColor("dark orange"), List((Amount(3), BagColor("bright white")), (Amount(4), BagColor("muted yellow")))),
          BagColorRule(BagColor("bright white"), List((Amount(1), BagColor("shiny gold")))),
          BagColorRule(BagColor("muted yellow"), List((Amount(2), BagColor("shiny gold")), (Amount(9), BagColor("faded blue")))),
          BagColorRule(BagColor("shiny gold"), List((Amount(1), BagColor("dark olive")), (Amount(2), BagColor("vibrant plum")))),
          BagColorRule(BagColor("dark olive"), List((Amount(3), BagColor("faded blue")), (Amount(4), BagColor("dotted black")))),
          BagColorRule(BagColor("vibrant plum"), List((Amount(5), BagColor("faded blue")), (Amount(6), BagColor("dotted black")))),
          BagColorRule(BagColor("faded blue"), List()),
          BagColorRule(BagColor("dotted black"), List())
        )
      ).countBags(BagColor("shiny gold"))
      assert(count)(equalTo(32L))
    },
    test("bag count for shiny gold bag in example 2") {
      val count = BagColorRelations(
        List(
          BagColorRule(BagColor("shiny gold"), List((Amount(2), BagColor("dark red")))),
          BagColorRule(BagColor("dark red"), List((Amount(2), BagColor("dark orange")))),
          BagColorRule(BagColor("dark orange"), List((Amount(2), BagColor("dark yellow")))),
          BagColorRule(BagColor("dark yellow"), List((Amount(2), BagColor("dark green")))),
          BagColorRule(BagColor("dark green"), List((Amount(2), BagColor("dark blue")))),
          BagColorRule(BagColor("dark blue"), List((Amount(2), BagColor("dark violet")))),
          BagColorRule(BagColor("dark violet"), List())
        )
      ).countBags(BagColor("shiny gold"))
      assert(count)(equalTo(126L))
    }
  )
}
