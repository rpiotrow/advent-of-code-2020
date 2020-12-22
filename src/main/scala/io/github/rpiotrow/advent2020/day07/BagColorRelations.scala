package io.github.rpiotrow.advent2020.day07

class BagColorRelations(map: Map[BagColor, List[(Amount, BagColor)]]) {

  def countBags(initial: BagColor): Long = {
    def helper(bagColor: BagColor, amount: Long): Long = {
      val newPaths = map.getOrElse(bagColor, List.empty)

      amount + amount * newPaths.foldLeft(0L) {
        case (acc, (childAmount, childColor)) => acc + helper(childColor, childAmount.value)
      }
    }

    helper(initial, 1L) - 1
  }

}

object BagColorRelations {
  def apply(list: List[BagColorRule]) = new BagColorRelations(list.map(_.pair).toMap)
}