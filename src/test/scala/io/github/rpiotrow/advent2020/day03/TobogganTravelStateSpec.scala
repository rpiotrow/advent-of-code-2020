package io.github.rpiotrow.advent2020.day03

import zio.test.Assertion.equalTo
import zio.test._

object TobogganTravelStateSpec extends DefaultRunnableSpec {
  def spec = suite("day03: TobogganTravelState")(
    testM("..##....... x=0") {
      for {
        treeRow <- TreeRow.fromString("..##.......")
        result = TobogganTravelState().down(treeRow, 3)
      } yield assert(result)(equalTo(TobogganTravelState(3, 1)))
    },
    testM("..##....... 0 x=3") {
      for {
        treeRow <- TreeRow.fromString("..##.......")
        result = TobogganTravelState(3, 1).down(treeRow, 3)
      } yield assert(result)(equalTo(TobogganTravelState(6, 1)))
    },
    testM("..##....... 0 x=9") {
      for {
        treeRow <- TreeRow.fromString("..##.......")
        result = TobogganTravelState(9, 1).down(treeRow, 3)
      } yield assert(result)(equalTo(TobogganTravelState(12, 1)))
    },
    testM("..##....... 0 x=10") {
      for {
        treeRow <- TreeRow.fromString("..##.......")
        result = TobogganTravelState(10, 1).down(treeRow, 3)
      } yield assert(result)(equalTo(TobogganTravelState(13, 2)))
    }
  )
}
