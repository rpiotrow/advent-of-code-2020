package io.github.rpiotrow.advent2020.day07

import scala.annotation.tailrec

case class BagColorReversedRelations(map: Map[BagColor, Set[BagColor]]) extends AnyVal {
  def eventualBagColors(color: BagColor): Set[BagColor] = {
    @tailrec
    def helper(work: List[BagColor], acc: Set[BagColor]): Set[BagColor] =
      if (work.isEmpty)
        acc
      else {
        val color :: workTail = work
        val newColors = get(color).diff(acc)
        helper(workTail.prependedAll(newColors), acc ++ newColors)
      }

    helper(List(color), Set.empty)
  }

  private def get(key: BagColor): Set[BagColor] = map.getOrElse(key, Set.empty)
}

object BagColorReversedRelations {
  def fromReversedPairs(list: List[(BagColor, BagColor)]): BagColorReversedRelations =
    BagColorReversedRelations(
      list
        .groupBy(_._1)
        .view.mapValues(_.map(_._2).toSet)
        .toMap
    )
}