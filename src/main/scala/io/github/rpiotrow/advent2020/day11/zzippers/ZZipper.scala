package io.github.rpiotrow.advent2020.day11.zzippers

import cats.Comonad
import io.github.rpiotrow.advent2020.day11.zzippers.ZZipper.{ZZipperMoveFailure, _}
import zio._
import zio.stream._

case class ZZipper[A](left: ZList[A], focus: A, right: ZList[A]) {
  def moveLeft: IO[ZZipperMoveFailure, ZZipper[A]] = {
    left.peel(ZSink.head[A]).use({
      case (h, z) =>
        for {
          hh <- ZIO.fromOption(h).orElseFail(MoveNotPossible)
        } yield ZZipper(z, hh, ZStream(focus) ++ right)
    })
  }
  def moveRight: IO[ZZipperMoveFailure, ZZipper[A]] = {
    right.peel(ZSink.head[A]).use({
      case (h, z) =>
        for {
          hh <- ZIO.fromOption(h).orElseFail(MoveNotPossible)
        } yield ZZipper(ZStream(focus) ++ left, hh, z)
    })
  }

  def mapMove[B](faz: A => IO[ZZipperMoveFailure, B]): IO[ZZipperMoveFailure, ZZipper[B]] =
    faz(focus).map { f =>
      ZZipper(left.mapM(faz), f, right.mapM(faz))
    }

  def mapM[B](faz: A => UIO[B]): UIO[ZZipper[B]] = {
    faz(focus).map { f =>
      ZZipper(left.mapM(faz), f, right.mapM(faz))
    }
  }

  def getManyTimesLeft: UIO[String] = {
    for {
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      _ <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
      n <- moveLeft.map(_.focus).option.map(_.fold("n")(_ => "y"))
    } yield s"""(n=$n)"""
  }

}

object ZZipper {

  trait ZZipperMoveFailure
  case object MoveNotPossible extends ZZipperMoveFailure

  type ZList[A] = ZStream[Any, ZZipperMoveFailure, A]

  implicit val zZipperComonad: Comonad[ZZipper] = new Comonad[ZZipper] {
    override def extract[A](zipper: ZZipper[A]): A =
      zipper.focus

    override def coflatMap[A, B](zipper: ZZipper[A])(f: ZZipper[A] => B): ZZipper[B] =
      map(duplicate(zipper))(f)

    override def map[A, B](zipper: ZZipper[A])(f: A => B): ZZipper[B] =
      ZZipper(zipper.left.map(f), f(zipper.focus), zipper.right.map(f))

    private def duplicate[A](zipper: ZZipper[A]): ZZipper[ZZipper[A]] = {
      val duplicateLefts: ZList[ZZipper[A]] =
        ZStream.unfoldM(zipper)(z => z.moveLeft.map(x => (x, x)).option)
      val duplicateRights: ZList[ZZipper[A]] =
        ZStream.unfoldM(zipper)(z => z.moveRight.map(x => (x, x)).option)
      ZZipper(duplicateLefts, zipper, duplicateRights)
    }
  }

  implicit val zZipperCountable: Countable[ZZipper] =
    new Countable[ZZipper] {
      override def count[A](zzipper: ZZipper[A])(f: A => Boolean): UIO[Long] = {
        for {
          l <- zzipper.left.filter(f).either.takeWhile(_.isRight).runCount
          fc = if (f(zzipper.focus)) 1 else 0
          r <- zzipper.right.filter(f).either.takeWhile(_.isRight).runCount
        } yield l + fc + r
      }
    }

  def fromList[A](list: List[A]): IO[String, ZZipper[A]] =
    if (list.isEmpty)
      ZIO.fail("list is empty")
    else
      ZIO.succeed(unsafeFromList(list))

  def unsafeFromList[A](list: List[A]): ZZipper[A] = {
    val rev = list.reverse
    ZZipper(ZStream.fromIterable(rev.tail), rev.head, ZStream.empty)
  }
}
//case class ZZipper[A](left: ZList[A], focus: A, right: ZList[A]) {
//
//  def moveLeft: IO[ZZipperMoveFailure, ZZipper[A]] = {
//    val s = ZStream(1,2,3)
//    s.tap()
//
//    (for {
//      aHead <- left.head
//      aTail <- left.tail
//      zipper = ZZipper(aTail, aHead, ZList(focus, right))
//    } yield zipper).orElseFail(MoveNotPossible)
//  }
//
//  def moveRight: IO[ZZipperMoveFailure, ZZipper[A]] =
//    (for {
//      aHead <- right.head
//      aTail <- right.tail
//      zipper = ZZipper(ZList(focus, right), aHead, aTail)
//    } yield zipper).orElseFail(MoveNotPossible)
//
//  def mapM[R, E, B](faz: A => ZIO[R, E, B]): ZIO[R, E, ZZipper[B]] = {
//    for {
//      l <- left.mapM(faz)
//      f <- faz(focus)
//      r <- right.mapM[R, E, B](faz)
//    } yield ZZipper(l, f, r)
//  }
//}
//
//object ZZipper {
//  trait ZZipperMoveFailure
//  case object MoveNotPossible extends ZZipperMoveFailure
//
//  def fromList[A](list: List[A]): IO[String, ZZipper[A]] =
//    if (list.isEmpty)
//      ZIO.fail("list is empty")
//    else
//      ZIO.succeed(unsafeFromList(list))
//
//  def unsafeFromList[A](list: List[A]): ZZipper[A] = {
//    val rev = list.reverse
//    ZZipper(ZList.from(rev.tail), rev.head, ZList.empty)
//  }
//
//  implicit val zZipperComonad: Comonad[ZZipper] = new Comonad[ZZipper] {
//    override def extract[A](zipper: ZZipper[A]): A =
//      zipper.focus
//
//    override def coflatMap[A, B](zipper: ZZipper[A])(f: ZZipper[A] => B): ZZipper[B] =
//      map(duplicate(zipper))(f)
//
//    override def map[A, B](zipper: ZZipper[A])(f: A => B): ZZipper[B] =
//      ZZipper(zipper.left.map(f), f(zipper.focus), zipper.right.map(f))
//
//    private def duplicate[A](zipper: ZZipper[A]): ZZipper[ZZipper[A]] = {
//      val duplicateLefts: ZList[ZZipper[A]] =
//        ZList.unfold(zipper)(z => z.moveLeft.bimap(_ => ZListEmpty, x => (x, x)))
//      val duplicateRights: ZList[ZZipper[A]] =
//        ZList.unfold(zipper)(z => z.moveRight.bimap(_ => ZListEmpty, x => (x, x)))
//      ZZipper(duplicateLefts, zipper, duplicateRights)
//    }
//  }
//  implicit val zZipperCountable: Countable[ZZipper] =
//    new Countable[ZZipper] {
//      override def count[A](zzipper: ZZipper[A])(f: A => Boolean): Int = {
//        zzipper.left.count(f) + (if (f(zzipper.focus)) 1 else 0) + zzipper.right.count(f)
//      }
//    }
//}

