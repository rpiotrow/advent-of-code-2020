package io.github.rpiotrow.advent2020.day11.zzippers

//import cats.Comonad
import cats.implicits._
import io.github.rpiotrow.advent2020.day11.zzippers.GridZZipper._
import io.github.rpiotrow.advent2020.day11.zzippers.Countable.CountableOps
import io.github.rpiotrow.advent2020.day11.zzippers.ZZipper.{ZList, ZZipperMoveFailure}
import zio._
import zio.stream.ZStream

case class GridZZipper[A](value: ZZipper[ZZipper[A]]) {
  def getNeighbors: UIO[List[A]] = {
    ZIO.collectAllSuccesses(
      List(
        north,
        east,
        south,
        west,
        northEast,
        northWest,
        southEast,
        southWest
      ).map(e => e.map(g => g.extract))
    )
  }
  def getManyTimesNorth: UIO[String] = {
    for {
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      _ <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
      n <- north.map(_.extract).option.map(_.fold("n")(_ => "y"))
    } yield s"""(n=$n)"""
  }

  def getVisibles(transparent: A): UIO[List[A]] = {
    case object ElementNotFound
    def firstInDirection(direction: GridZZipper[A] => IO[GridZZipperMoveFailure, GridZZipper[A]]): IO[ElementNotFound.type, A] =
      (for {
        option <- ZStream
          .unfoldM(this)(direction(_).map(x => (x, x).some))
          .map(_.extract)
          .filter(_ != transparent)
          .runHead
        r <- ZIO.fromOption(option).orElseFail(ElementNotFound)
      } yield r).orElseFail(ElementNotFound)

    ZIO.collectAllSuccesses(
      List(
        firstInDirection(_.north),
        firstInDirection(_.east),
        firstInDirection(_.south),
        firstInDirection(_.west),
        firstInDirection(_.northEast),
        firstInDirection(_.northWest),
        firstInDirection(_.southEast),
        firstInDirection(_.southWest)
      )
    )
  }

  def extract: A = value.focus.focus

  def map[B](f: A => B): GridZZipper[B] =
    GridZZipper(value.map(_.map(f)))

  def mapM[B](f: A => UIO[B]): UIO[GridZZipper[B]] = {
    value.mapM(_.mapM(f)).map(GridZZipper(_))
  }

  def coflatMapMove[B](f: GridZZipper[A] => UIO[B]): UIO[GridZZipper[B]] = {
    duplicate.mapM(f)
  }

  private def duplicate: GridZZipper[GridZZipper[A]] =
    GridZZipper(nest(nest(value))).map(GridZZipper(_))

  private def nest[B](s: ZZipper[ZZipper[B]]): ZZipper[ZZipper[ZZipper[B]]] = {
    val duplicateLefts: ZList[ZZipper[ZZipper[B]]] =
      ZStream.unfoldM(s)(zipper =>
        zipper.mapMove(_.moveLeft).map(x => (x, x)).option
      )
    val duplicateRights: ZList[ZZipper[ZZipper[B]]] =
      ZStream.unfoldM(s)(zipper =>
        zipper.mapMove(_.moveRight).map(x => (x, x)).option
      )
    ZZipper(duplicateLefts, s, duplicateRights)
  }

   def north: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    value.moveLeft.bimap(_ => MoveNotPossible, GridZZipper(_))
   def south: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    value.moveRight.bimap(_ => MoveNotPossible, GridZZipper(_))
   def east: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    value.mapMove(xAxis => xAxis.moveRight).bimap(_ => MoveNotPossible, GridZZipper(_))
   def west: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    value.mapMove(xAxis => xAxis.moveLeft).bimap(_ => MoveNotPossible, GridZZipper(_))

   def northEast: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    north.flatMap(_.east)
   def northWest: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    north.flatMap(_.west)
   def southEast: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    south.flatMap(_.east)
   def southWest: IO[GridZZipperMoveFailure, GridZZipper[A]] =
    south.flatMap(_.west)
}

object GridZZipper {

  trait GridZZipperMoveFailure
  case object MoveNotPossible extends GridZZipperMoveFailure

  def fromList[A](list: List[List[A]]): IO[String, GridZZipper[A]] = {
    for {
      listOfZippers <- ZIO.foreach(list)(ZZipper.fromList)
      zipperOfZippers <- ZZipper.fromList(listOfZippers)
    } yield GridZZipper(zipperOfZippers)
  }

  def unsafeFromList[A](list: List[List[A]]): GridZZipper[A] = {
    val listOfZippers = list.map(ZZipper.unsafeFromList)
    val zipperOfZippers = ZZipper.unsafeFromList(listOfZippers)
    GridZZipper(zipperOfZippers)
  }


  //  implicit val gridZZipperComonad: Comonad[GridZZipper] = {
//    new Comonad[GridZZipper] {
//      override def extract[A](w: GridZZipper[A]): A =
//        w.value.focus.focus
//
//      override def map[A, B](fa: GridZZipper[A])(f: A => B): GridZZipper[B] =
//        GridZZipper(fa.value.map(_.map(f)))
//
//      override def coflatMap[A, B](w: GridZZipper[A])(f: GridZZipper[A] => B): GridZZipper[B] =
//        map(duplicate(w))(f)
//
//      private def duplicate[A](w: GridZZipper[A]): GridZZipper[GridZZipper[A]] =
//        map(GridZZipper(nest(nest(w.value))))(GridZZipper(_))
//
//      private def nest[A](s: ZZipper[ZZipper[A]]): ZZipper[ZZipper[ZZipper[A]]] = {
//        val duplicateLefts: ZList[ZZipper[ZZipper[A]]] =
//          ZStream.unfoldM(s)(zipper =>
//            zipper.mapMove(_.moveLeft).map(x => Some((x, x)))
//          )
//        val duplicateRights: ZList[ZZipper[ZZipper[A]]] =
//          ZStream.unfoldM(s)(zipper =>
//            zipper.mapMove(_.moveRight).map(x => Some((x, x)))
//          )
//        ZZipper(duplicateLefts, s, duplicateRights)
//      }
//    }
//  }
  implicit val gridZZipperCountable: Countable[GridZZipper] =
    new Countable[GridZZipper] {
      override def count[A](grid: GridZZipper[A])(f: A => Boolean): UIO[Long] = {
        for {
          l <- grid.value.left.either.takeWhile(_.isRight).map(_.toOption.get).mapM(e => e.count(f)).runSum
          fc <- grid.value.focus.count(f)
          r <- grid.value.right.either.takeWhile(_.isRight).map(_.toOption.get).mapM(_.count(f)).runSum
        } yield l + fc + r
      }
    }
}
