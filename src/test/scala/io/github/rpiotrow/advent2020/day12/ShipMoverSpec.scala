package io.github.rpiotrow.advent2020.day12

import zio.test._
import zio.test.Assertion.equalTo

object ShipMoverSpec extends DefaultRunnableSpec {
  override def spec = suite("day 12: ShipMoverSpec")(
    test("example instructions") {
      val endLocation = ShipMover.move(
        Ship(ShipLocation(0, 0), East),
        List(
          MoveForward(10),
          Move(North, 3),
          MoveForward(7),
          Turn(RightDirection, Degrees90),
          MoveForward(11)
        )
      )
      assert(endLocation)(equalTo(Ship(ShipLocation(17, -8), South)))
    }
  )
}
