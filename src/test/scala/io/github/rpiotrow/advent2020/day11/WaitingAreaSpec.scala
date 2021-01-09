package io.github.rpiotrow.advent2020.day11

import zio._
import zio.test.Assertion.equalTo
import zio.test._

object WaitingAreaSpec extends DefaultRunnableSpec {
  override def spec = suite("day 11: WaitingAreaSpec")(
    testM("waiting area example occupied count") {
      (for {
        input <- ZIO.collectAll(List(
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
        grid <- GridZipper.fromList(input)
        occupiedCount <- WaitingArea(grid).occupiedCountWhenStable
      } yield assert(occupiedCount)(equalTo(37)))
    }
  )
}
