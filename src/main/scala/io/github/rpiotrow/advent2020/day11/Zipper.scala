package io.github.rpiotrow.advent2020.day11

import cats.Comonad
import zio._

case class Zipper[A](left: LazyList[A], focus: A, right: LazyList[A]) {

  def maybeMoveLeft: Option[Zipper[A]] =
    if (left.isEmpty) None else Some(unsafeMoveLeft)
  def maybeMoveRight: Option[Zipper[A]] =
    if (right.isEmpty) None else Some(unsafeMoveRight)

  def unsafeMoveLeft: Zipper[A] =
    Zipper(left.tail, left.head, focus #:: right)
  def unsafeMoveRight: Zipper[A] =
    Zipper(focus #:: left, right.head, right.tail)
}

object Zipper extends ZipperInstances {
  def fromList[A](list: List[A]): IO[String, Zipper[A]] =
    if (list.isEmpty)
      ZIO.fail("list is empty")
    else
      ZIO.succeed(unsafeFromList(list))

  def unsafeFromList[A](list: List[A]): Zipper[A] = {
    val rev = list.reverse
    Zipper(LazyList.from(rev.tail), rev.head, LazyList.empty)
  }
}

trait ZipperInstances {
  implicit val zipperComonad: Comonad[Zipper] = new Comonad[Zipper] {
    override def extract[A](zipper: Zipper[A]): A =
      zipper.focus

    override def coflatMap[A, B](zipper: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      map(duplicate(zipper))(f)

    override def map[A, B](zipper: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(zipper.left.map(f), f(zipper.focus), zipper.right.map(f))

    private def duplicate[A](zipper: Zipper[A]): Zipper[Zipper[A]] = {
      val duplicateLefts: LazyList[Zipper[A]] =
        LazyList.unfold(zipper)(z => z.maybeMoveLeft.map(x => (x, x)))
      val duplicateRights: LazyList[Zipper[A]] =
        LazyList.unfold(zipper)(z => z.maybeMoveRight.map(x => (x, x)))
      Zipper(duplicateLefts, zipper, duplicateRights)
    }
  }

  implicit val zipperCountable: Countable[Zipper] =
    new Countable[Zipper] {
      override def count[A](zipper: Zipper[A])(f: A => Boolean): Int =
        zipper.left.count(f) + (if (f(zipper.focus)) 1 else 0) + zipper.right.count(f)
    }
}