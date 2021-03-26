package io.github.rpiotrow.advent2020.day12

import zio.test.Assertion.equalTo
import zio.test._

object WaypointSpec extends DefaultRunnableSpec {
  private val waypoint = Waypoint(3,2)
  override def spec =
    suite("day 12: WaypointSpec")(
      test("waypoint rotate left 90") {
        assert(waypoint.rotate(LeftDirection, Degrees90))(equalTo(Waypoint(-2, 3)))
      },
      test("waypoint rotate left 180") {
        assert(waypoint.rotate(LeftDirection, Degrees180))(equalTo(Waypoint(-3, -2)))
      },
      test("waypoint rotate left 270") {
        assert(waypoint.rotate(LeftDirection, Degrees270))(equalTo(Waypoint(2, -3)))
      },
      test("waypoint rotate right 90") {
        assert(waypoint.rotate(RightDirection, Degrees90))(equalTo(Waypoint(2, -3)))
      },
      test("waypoint rotate right 180") {
        assert(waypoint.rotate(RightDirection, Degrees180))(equalTo(Waypoint(-3, -2)))
      },
      test("waypoint rotate right 270") {
        assert(waypoint.rotate(RightDirection, Degrees270))(equalTo(Waypoint(-2, 3)))
      }
    )
}
