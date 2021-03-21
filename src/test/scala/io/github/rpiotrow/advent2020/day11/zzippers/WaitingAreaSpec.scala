package io.github.rpiotrow.advent2020.day11.zzippers

import io.github.rpiotrow.advent2020.day11.Cell
import io.github.rpiotrow.advent2020.day11.zzippers.ZZipper.{MoveNotPossible, ZList}
import zio.stream._
import zio._
import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

import cats.implicits._

object WaitingAreaSpec extends DefaultRunnableSpec {
//  private val inputList = ZIO.collectAll(List(
//    Cell.listFromString("L.LL.LL.LL"),
//    Cell.listFromString("LLLLLLL.LL"),
//    Cell.listFromString("L.L.L..L.."),
//    Cell.listFromString("LLLL.LL.LL"),
//    Cell.listFromString("L.LL.LL.LL"),
//    Cell.listFromString("L.LLLLL.LL"),
//    Cell.listFromString("..L.L....."),
//    Cell.listFromString("LLLLLLLLLL"),
//    Cell.listFromString("L.LLLLLL.L"),
//    Cell.listFromString("L.LLLLL.LL")
//  ))
  private val inputList = ZIO.collectAll(List(
    Cell.listFromString("#.##.##.##"),
    Cell.listFromString("#######.##"),
    Cell.listFromString("#.#.#..#.."),
    Cell.listFromString("####.##.##"),
    Cell.listFromString("#.##.##.##"),
    Cell.listFromString("#.#####.##"),
    Cell.listFromString("..#.#....."),
    Cell.listFromString("##########"),
    Cell.listFromString("#.######.#"),
    Cell.listFromString("#.#####.##")
  ))
  private val inputCharList = ZIO.succeed(List(
    "#.##.##.##".toList,
    "#######.##".toList,
    "#.#.#..#..".toList,
    "####.##.##".toList,
    "#.##.##.##".toList,
    "#.#####.##".toList,
    "..#.#.....".toList,
    "##########".toList,
    "#.######.#".toList,
    "#.#####.##".toList
  ))

  def putStr[A](header: String)(grid: GridZZipper[A]): RIO[console.Console, Unit] = {
    for {
      l <- zzipperToList(grid.value)
      ll <- ZIO.foreach(l)(zzipperToList)
      _ <- console.putStrLn(header)
      _ <- console.putStrLn(ll.map(_.mkString).mkString("\n"))
    } yield ()
  }

  def putStrZ[A](header: String)(zipper: ZZipper[A]): RIO[console.Console, Unit] = {
    for {
      l <- zzipperToList(zipper)
      _ <- console.putStrLn(header)
      _ <- console.putStrLn(l.mkString(","))
    } yield ()
  }

  private def zzipperToList[A](z: ZZipper[A]): UIO[List[A]] = {
    for {
      l <- z.left.either.takeWhile(_.isRight).map(_.toOption.get).runCollect.map(_.toList)
      c = z.focus
      r <- z.right.either.takeWhile(_.isRight).map(_.toOption.get).runCollect.map(_.toList)
    } yield (l.reverse) ++ (c :: r)
  }

  private def zlistToList[A](z: ZList[A]): UIO[List[A]] =
    z.either.takeWhile(_.isRight).map(_.toOption.get).runCollect.map(_.toList)

  /*
  input:
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
   */

  /*
  should be:
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
   */

  /*
  is:
#.LL.L#.##
#LLL#L#.##
#.L.L..#..
LLLL.LL.L#
#.LL.LL.LL
#.LLLL#.L#
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
   */

  // TODO: check if ZZiper with ZStream is a value or is mutable (creature)

  override def spec = suite("day 11: WaitingAreaSpec")(
    testM("WTF1") {
      for {
        grid <- ZZipper.fromList("#.##.##.##".toList)
        _ <- putStrZ("T1")(grid)
        g = grid.coflatMap(g =>
          g.getManyTimesLeft
        )
        _ <- putStrZ("T2")(g)
      } yield assert(1L)(equalTo(1L))
    },
    testM("test testu") {
      val s = for {
        input <- inputCharList
        grid <- GridZZipper.fromList(input)
        _ <- putStr("T1")(grid)
        g <- grid.coflatMapMove(g =>
          // g.northWest.option.map(_.fold('-')(_ => '+'))
           //g.getNeighbors.map(l => s"(${l.mkString(",")})")
          g.getManyTimesNorth
//          ZIO.succeed(g.extract)
        )
//        g = grid
        _ <- putStr("T2")(g)

//        _ <- zio.console.putStrLn("c0")
//
        g2 <- grid.north
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.west)
        g2n <- g.north
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.north)
          .flatMap(_.west)
        ll <- g2.getNeighbors
        _ <- zio.console.putStrLn(s"ll ${ll.size}")
        _ <- zio.console.putStrLn(s"ll ${g2n.extract}")
        n <- g2.north
        s <- g2.south
        w <- g2.west
        e <- g2.east
        nw <- g2.northWest
        se <- g2.southEast
        ne <- g2.northEast
        ss <- g2.southWest
//
//        c1 <- zlistToList(g2.value.left).map(_.size)
//        _ <- zio.console.putStrLn(s"c1 = $c1")
////        c2 <- zzipperToList(g2.value).map(_.size)
////        _ <- zio.console.putStrLn(s"c2 = $c2")
////        c3 <- zzipperToList(g2.value).map(_.size)
////        _ <- zio.console.putStrLn(s"c3 = $c3")
//
//        _ <- g2.value.left.runHead

//        _ <- g2.value.left.peel(ZSink.head[ZZipper[Int]]).use({
//          case (h, z) =>
//            for {
//              hh <- ZIO.fromOption(h).orElseFail(MoveNotPossible)
//            } yield ZZipper(z, hh, ZStream(g2.value.focus) ++ g2.value.right)
//        })
//        _ <- g2.north

//        c2 <- zzipperToList(g2.value).map(_.size)
//        _ <- zio.console.putStrLn(s"c2 = $c2")
//        _ <- g2.north
//        c3 <- zzipperToList(g2.value).map(_.size)
//        _ <- zio.console.putStrLn(s"c3 = $c3")
//        _ <- g2.north
//        c4 <- zzipperToList(g2.value).map(_.toList.size)
//        _ <- zio.console.putStrLn(s"c4 = $c4")
//
        //          _ <- zio.console.putStrLn(s"WAT ? ${i.toString}")
      } yield assert(1L)(equalTo(1L))
      s
    }
    ,
    testM("waiting area example occupied count using neighboursRules") {
      (for {
        input <- inputList
        grid <- GridZZipper.fromList(input)
        occupiedCount <- WaitingArea(grid).occupiedCountWhenStable(LifecycleRules.neighboursRules)
      } yield assert(occupiedCount)(equalTo(37L)))
    }
//    ,
//    testM("waiting area example occupied count using visiblesRules") {
//      (for {
//        input <- inputList
//        grid <- GridZZipper.fromList(input)
//        occupiedCount <- WaitingArea(grid).occupiedCountWhenStable(LifecycleRules.visibilityRules)
//      } yield assert(occupiedCount)(equalTo(26L)))
//    }
  )
}
