package io.github.rpiotrow.advent2020.day07

import zio.test.Assertion.equalTo
import zio.test._

object BagColorReversedRelationsSpec extends DefaultRunnableSpec {
  def spec = suite("day 07: BagColorReversedRelationsSpec")(
    test("eventually bag colors count for shiny gold bag") {
      val count = BagColorReversedRelations.fromReversedPairs(
        List(
          BagColor("muted yellow") -> BagColor("light red"),
          BagColor("bright white") -> BagColor("light red"),
          BagColor("muted yellow") -> BagColor("dark orange"),
          BagColor("bright white") -> BagColor("dark orange"),
          BagColor("shiny gold") -> BagColor("bright white"),
          BagColor("faded blue") -> BagColor("muted yellow"),
          BagColor("shiny gold") -> BagColor("muted yellow"),
          BagColor("dark olive") -> BagColor("shiny gold"),
          BagColor("vibrant plum") -> BagColor("shiny gold"),
          BagColor("dotted black") -> BagColor("dark olive"),
          BagColor("faded blue") -> BagColor("dark olive"),
          BagColor("faded blue") -> BagColor("vibrant plum"),
          BagColor("dotted black") -> BagColor("vibrant plum")
        )
      ).eventualBagColors(BagColor("shiny gold")).size
      assert(count)(equalTo(4))
    }
  )
}
