package io.github.rpiotrow.advent2020.day12

import zio.test.Assertion.equalTo
import zio.test._

object ShipLocationSpec extends DefaultRunnableSpec {
  override def spec = suite("day 12: ShipLocationSpec")(
    test("manhattan distance from (17, 8) to (0, 0) is 25 ") {
      assert(ShipLocation(17, -8).manhattanDistanceToZero)(equalTo(25))
    },
    test("manhattan distance from (10, 3) is 13") {
      assert(ShipLocation(10, 3).manhattanDistanceToZero)(equalTo(13))
    },
    test("manhattan distance from (-10, 5) is 15") {
      assert(ShipLocation(-10, 5).manhattanDistanceToZero)(equalTo(15))
    },
    test("manhattan distance from (-7, -8) is 15") {
      assert(ShipLocation(-7, -8).manhattanDistanceToZero)(equalTo(15))
    },
    test("move from (1, 1) north 8 is (1, 9)") {
      assert(ShipLocation(1, 1).move(North, 8))(equalTo(ShipLocation(1, 9)))
    },
    test("move from (1, 1) south 8 is (1, -7)") {
      assert(ShipLocation(1, 1).move(South, 8))(equalTo(ShipLocation(1, -7)))
    },
    test("move from (1, 1) east 5 is (6, 1)") {
      assert(ShipLocation(1, 1).move(East, 5))(equalTo(ShipLocation(6, 1)))
    },
    test("move from (1, 1) west 5 is (-4, 1)") {
      assert(ShipLocation(1, 1).move(West, 5))(equalTo(ShipLocation(-4, 1)))
    }
  )
}
