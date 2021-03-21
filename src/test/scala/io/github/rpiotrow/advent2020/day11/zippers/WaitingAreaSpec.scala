package io.github.rpiotrow.advent2020.day11.zippers

import io.github.rpiotrow.advent2020.day11.Cell
import zio.ZIO
import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object WaitingAreaSpec extends DefaultRunnableSpec {
  private val inputList = ZIO.collectAll(List(
    Cell.listFromString("L.LL.LL.LL"),
    Cell.listFromString("LLLLLLL.LL"),
    Cell.listFromString("L.L.L..L.."),
    Cell.listFromString("LLLL.LL.LL"),
    Cell.listFromString("L.LL.LL.LL"),
    Cell.listFromString("L.LLLLL.LL"),
    Cell.listFromString("..L.L....."),
    Cell.listFromString("LLLLLLLLLL"),
    Cell.listFromString("L.LLLLLL.L"),
    Cell.listFromString("L.LLLLL.LL")
  ))

  override def spec = suite("day 11: WaitingAreaSpec")(
    testM("waiting area example occupied count using neighboursRules") {
      (for {
        input <- inputList
        grid <- GridZipper.fromList(input)
        occupiedCount <- WaitingArea(grid).occupiedCountWhenStable(LifecycleRules.neighboursRules)
      } yield assert(occupiedCount)(equalTo(37)))
    },
    testM("waiting area example occupied count using visiblesRules") {
      (for {
        input <- inputList
        grid <- GridZipper.fromList(input)
        occupiedCount <- WaitingArea(grid).occupiedCountWhenStable(LifecycleRules.visibilityRules)
      } yield assert(occupiedCount)(equalTo(26)))
    }
  )
}
