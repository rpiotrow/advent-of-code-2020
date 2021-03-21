package io.github.rpiotrow.advent2020.day11.zippers

import cats.implicits._
import cats.{Comonad, Show}
import io.github.rpiotrow.advent2020.day11.zippers.Countable.CountableOps
import zio._

case class GridZipper[A](value: Zipper[Zipper[A]]) {

  def getNeighbors: List[A] =
    List(
      maybeNorth,
      maybeEast,
      maybeSouth,
      maybeWest,
      maybeNorth.flatMap(_.maybeEast),
      maybeNorth.flatMap(_.maybeWest),
      maybeSouth.flatMap(_.maybeEast),
      maybeSouth.flatMap(_.maybeWest)
    ).mapFilter[A](_.map(_.extract))

  def getVisibles(transparent: A): List[A] = {
    def firstInDirection(direction: GridZipper[A] => Option[GridZipper[A]]): Option[A] =
      LazyList
        .unfold(this)(direction(_).map(x => (x, x)))
        .map(_.extract)
        .find(_ != transparent)

    List(
      firstInDirection(_.maybeNorth),
      firstInDirection(_.maybeEast),
      firstInDirection(_.maybeSouth),
      firstInDirection(_.maybeWest),
      firstInDirection(_.maybeNorthEast),
      firstInDirection(_.maybeNorthWest),
      firstInDirection(_.maybeSouthEast),
      firstInDirection(_.maybeSouthWest)
    ).mapFilter[A](identity)
  }

  private def maybeNorth: Option[GridZipper[A]] =
    value.maybeMoveLeft.map(GridZipper(_))
  private def maybeSouth: Option[GridZipper[A]] =
    value.maybeMoveRight.map(GridZipper(_))
  private def maybeEast: Option[GridZipper[A]] =
    Option.when(value.extract.right.nonEmpty)(GridZipper(value.map(_.unsafeMoveRight)))
  private def maybeWest: Option[GridZipper[A]] =
    Option.when(value.extract.left.nonEmpty)(GridZipper(value.map(_.unsafeMoveLeft)))

  private def maybeNorthEast: Option[GridZipper[A]] = maybeNorth.flatMap(_.maybeEast)
  private def maybeNorthWest: Option[GridZipper[A]] = maybeNorth.flatMap(_.maybeWest)
  private def maybeSouthEast: Option[GridZipper[A]] = maybeSouth.flatMap(_.maybeEast)
  private def maybeSouthWest: Option[GridZipper[A]] = maybeSouth.flatMap(_.maybeWest)
}

object GridZipper extends GridZipperInstances {
  def fromList[A](list: List[List[A]]): IO[String, GridZipper[A]] = {
    for {
      listOfZippers <- ZIO.foreach(list)(Zipper.fromList)
      zipperOfZippers <- Zipper.fromList(listOfZippers)
    } yield GridZipper(zipperOfZippers)
  }

  def unsafeFromList[A](list: List[List[A]]): GridZipper[A] = {
    val listOfZippers = list.map(Zipper.unsafeFromList)
    val zipperOfZippers = Zipper.unsafeFromList(listOfZippers)
    GridZipper(zipperOfZippers)
  }
}

trait GridZipperInstances {
  implicit val gridZipperComonad: Comonad[GridZipper] = {
    new Comonad[GridZipper] {
      override def extract[A](w: GridZipper[A]): A =
        w.value.focus.focus

      override def map[A, B](fa: GridZipper[A])(f: A => B): GridZipper[B] =
        GridZipper(fa.value.map(_.map(f)))

      override def coflatMap[A, B](w: GridZipper[A])(f: GridZipper[A] => B): GridZipper[B] =
        map(duplicate(w))(f)

      private def duplicate[A](w: GridZipper[A]): GridZipper[GridZipper[A]] =
        map(GridZipper(nest(nest(w.value))))(GridZipper(_))

      private def makePair[A](x: A): (A, A) = (x, x)

      private def nest[A](zippers: Zipper[Zipper[A]]): Zipper[Zipper[Zipper[A]]] = {
        val duplicateLefts: LazyList[Zipper[Zipper[A]]] =
          LazyList.unfold(zippers)(zipper =>
            Option.when(zipper.extract.left.nonEmpty)(makePair(zipper.map(_.unsafeMoveLeft)))
          )
        val duplicateRights: LazyList[Zipper[Zipper[A]]] =
          LazyList.unfold(zippers)(zipper =>
            Option.when(zipper.extract.right.nonEmpty)(makePair(zipper.map(_.unsafeMoveRight)))
          )
        Zipper(duplicateLefts, zippers, duplicateRights)
      }
    }
  }

  implicit val gridZipperCountable: Countable[GridZipper] =
    new Countable[GridZipper] {
      override def count[A](gridZipper: GridZipper[A])(f: A => Boolean): Int = {
        val leftCount = gridZipper.value.left.foldLeft(0)(_ + _.count(f))
        val focusCount = gridZipper.value.focus.count(f)
        gridZipper.value.right.foldLeft(leftCount + focusCount)(_ + _.count(f))
      }
    }

  implicit def gridZipperShow[A]: Show[GridZipper[A]] = new Show[GridZipper[A]] {
    override def show(grid: GridZipper[A]): String =
      toList(grid).map(list => list.mkString).mkString("\n")

    private def toList(grid: GridZipper[A]): List[List[A]] = {
      val leftLists = grid.value.left.map(toList).reverse.toList
      val rightLists = grid.value.right.map(toList).toList
      leftLists ++ (toList(grid.value.focus) :: rightLists)
    }

    private def toList(zipper: Zipper[A]): List[A] =
      zipper.left.reverse.toList ++ (zipper.focus :: zipper.right.toList)
  }
}
