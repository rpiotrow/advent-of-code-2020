package io.github.rpiotrow.advent2020.day03

import zio.test.Assertion.equalTo
import zio.test._

object TreeRowSpec extends DefaultRunnableSpec {
  def spec = suite("day03: TreeRowSpec")(
    testM("tree row from string ..##.......") {
      for {
        row <- TreeRow.fromString("..##.......")
      } yield assert(row.trees)(equalTo(Array(false, false, true, true, false, false, false, false, false, false, false)))
    },
    testM("tree row from string #...#...#..") {
      for {
        row <- TreeRow.fromString("#...#...#..")
      } yield assert(row.trees)(equalTo(Array(true, false, false, false, true, false, false, false, true, false, false)))
    },
    testM("tree row from string .#....#..#.") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
      } yield assert(row.trees)(equalTo(Array(false, true, false, false, false, false, true, false, false, true, false)))
    },
    testM("tree row .#....#..#. has tree at 1") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
        hasTree = row.hasTreeAt(1)
      } yield assert(hasTree)(equalTo(true))
    },
    testM("tree row .#....#..#. has not tree at 2") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
        hasTree = row.hasTreeAt(2)
      } yield assert(hasTree)(equalTo(false))
    }
  )
}
