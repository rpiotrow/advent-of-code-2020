package io.github.rpiotrow.advent2020.day11.zzippers

import cats.implicits._
import Countable.CountableOps
import io.github.rpiotrow.advent2020.day11.{Cell, OccupiedSeat}
import io.github.rpiotrow.advent2020.day11.zzippers.LifecycleRules.LifecycleRulesType
import io.github.rpiotrow.advent2020.day11.zzippers.WaitingArea.OccupiedCountAcc
import io.github.rpiotrow.advent2020.day11.zzippers.ZZipper.{MoveNotPossible, ZZipperMoveFailure}
import zio._
import zio.console.Console
import zio.stream.ZStream

case class WaitingArea(grid: GridZZipper[Cell]) {

  def occupiedCountWhenStable(lifecycleRules: LifecycleRulesType): ZIO[Console, String, Long] = {
    for {
      thisc <- this.occupiedCount
      s <- ZStream
        .unfoldM(this)(_.nextGeneration(lifecycleRules).map(x => Some((x, x))))
        .mapAccumM(OccupiedCountAcc(0, thisc))((acc, waitingArea) => {
          for {
            cc <- waitingArea.occupiedCount
            x = OccupiedCountAcc(previous = acc.current, current = cc)
          } yield (x, x)
        })
        .map({
          case o @ OccupiedCountAcc(a, b) => {
            println(s"a = $a b = $b")
            o
          }
        })
        .takeUntil(acc => acc.previous > 0 && acc.current == acc.previous)
        .runLast
        .flatMap(ZIO.fromOption(_)).bimap(_ => "solution not found", _.current)
    } yield s
  }

  def occupiedCount: UIO[Long] = grid.count(_ == OccupiedSeat)

  def nextGeneration(lifecycleRules: LifecycleRulesType): ZIO[Console, String, WaitingArea] = {
//    for {
//      z <- grid.coflatMapMove(lifecycleRules)
//      l <- WaitingArea(z).toList
//      g <- GridZZipper.fromList(l)
//      wa = WaitingArea(g)
//      _ <- wa.putStr("")
//    } yield wa
    grid.coflatMapMove(lifecycleRules).map(WaitingArea(_))
  }

  def toList: UIO[List[List[Cell]]] =
    for {
      l <- zzipperToList(grid.value)
      ll <- ZIO.foreach(l)(zzipperToList)
    } yield ll


  def putStr(header: String): URIO[console.Console, Unit] = {
    for {
      l <- zzipperToList(grid.value)
      ll <- ZIO.foreach(l)(zzipperToList)
      _ <- console.putStrLn(header)
      _ <- console.putStrLn(ll.map(_.mkString).mkString("\n"))
    } yield ()
  }

  private def zzipperToList[A](z: ZZipper[A]): UIO[List[A]] = {
    for {
      l <- z.left.either.takeWhile(_.isRight).map(_.toOption.get).runCollect.map(_.toList)
      c = z.focus
      r <- z.right.either.takeWhile(_.isRight).map(_.toOption.get).runCollect.map(_.toList)
    } yield (l.reverse) ++ (c :: r)
  }
}

object WaitingArea {

  private case class OccupiedCountAcc(previous: Long, current: Long)

}