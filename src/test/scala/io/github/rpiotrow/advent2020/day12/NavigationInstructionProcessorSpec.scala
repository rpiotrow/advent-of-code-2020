package io.github.rpiotrow.advent2020.day12

import zio.stream.ZStream
import zio.test._
import zio.test.Assertion.equalTo

object NavigationInstructionProcessorSpec extends DefaultRunnableSpec {
  private val exampleInstructions = ZStream.fromIterable(
    List(
      MoveForward(10),
      Move(North, 3),
      MoveForward(7),
      Turn(RightDirection, Degrees90),
      MoveForward(11)
    )
  )
  override def spec =
    suite("day 12: NavigationInstructionProcessorSpec")(
      testM("direct ship example instructions") {
        for {
          endLocation <- NavigationInstructionProcessor.processInstructions(
            DirectShip(Location(0, 0), East),
            exampleInstructions
          )
        } yield assert(endLocation)(equalTo(DirectShip(Location(17, -8), South)))
      },
      testM("ship with Waypoint example instructions") {
        for {
          endLocation <- NavigationInstructionProcessor.processInstructions(
            ShipWithWaypoint(Location(0, 0), Waypoint(10, 1)),
            exampleInstructions
          )
        } yield assert(endLocation)(equalTo(ShipWithWaypoint(Location(214, -72), Waypoint(4, -10))))
      }
    )
}
