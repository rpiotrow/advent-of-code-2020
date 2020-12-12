package io.github.rpiotrow.advent2020.day03

import zio.test.Assertion.equalTo
import zio.test._

object TreeRowSpec extends DefaultRunnableSpec {
  def spec = suite("TreeRowSpec")(
    testM("..##.......") {
      for {
        row <- TreeRow.fromString("..##.......")
      } yield assert(row.trees)(equalTo(Array(false, false, true, true, false, false, false, false, false, false, false)))
    },
    testM("#...#...#..") {
      for {
        row <- TreeRow.fromString("#...#...#..")
      } yield assert(row.trees)(equalTo(Array(true, false, false, false, true, false, false, false, true, false, false)))
    },
    testM(".#....#..#.") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
      } yield assert(row.trees)(equalTo(Array(false, true, false, false, false, false, true, false, false, true, false)))
    },
    testM(".#....#..#. hasTreeAt(1)") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
        hasTree = row.hasTreeAt(1)
      } yield assert(hasTree)(equalTo(true))
    },
    testM(".#....#..#. hasTreeAt(2)") {
      for {
        row <- TreeRow.fromString(".#....#..#.")
        hasTree = row.hasTreeAt(2)
      } yield assert(hasTree)(equalTo(false))
    }
  )
}
