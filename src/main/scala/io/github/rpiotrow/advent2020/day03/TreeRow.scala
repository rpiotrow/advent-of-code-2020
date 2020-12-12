package io.github.rpiotrow.advent2020.day03

import zio._

case class TreeRow(trees: Array[Boolean]) {
  def hasTreeAt(x: Int): Boolean = trees(x % trees.length)
}

object TreeRow {
  def fromString(string: String): IO[String, TreeRow] =
    ZIO.collectAll(string.toList.map({
      case '#' => ZIO.succeed(true)
      case '.' => ZIO.succeed(false)
      case _ => ZIO.fail("invalid map")
    })).map(list => TreeRow(list.toArray))
}