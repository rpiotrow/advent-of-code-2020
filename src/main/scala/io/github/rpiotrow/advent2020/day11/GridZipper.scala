package io.github.rpiotrow.advent2020.day11

import cats.implicits._
import cats.{Comonad, Show}
import io.github.rpiotrow.advent2020.day11.UnorderedFoldableWithInitValue._
import zio._

case class GridZipper[A](value: Zipper[Zipper[A]]) {

  def maybeNorth: Option[GridZipper[A]] =
    value.maybeMoveLeft.map(GridZipper(_))
  def maybeSouth: Option[GridZipper[A]] =
    value.maybeMoveRight.map(GridZipper(_))
  def maybeEast: Option[GridZipper[A]] =
    if (value.extract.right.isEmpty) None
    else Some(GridZipper(value.map(_.unsafeMoveRight)))
  def maybeWest: Option[GridZipper[A]] =
    if (value.extract.left.isEmpty) None
    else Some(GridZipper(value.map(_.unsafeMoveLeft)))

  def getNeighbors: List[A] =
    List(
      this.maybeNorth,
      this.maybeEast,
      this.maybeSouth,
      this.maybeWest,
      this.maybeNorth.flatMap(_.maybeEast),
      this.maybeNorth.flatMap(_.maybeWest),
      this.maybeSouth.flatMap(_.maybeEast),
      this.maybeSouth.flatMap(_.maybeWest)
    ).mapFilter[A](_.map(_.extract))
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

      private def nest[A](s: Zipper[Zipper[A]]): Zipper[Zipper[Zipper[A]]] = {
        val duplicateLefts: LazyList[Zipper[Zipper[A]]] =
          LazyList.unfold(s)(zipper =>
            if (zipper.extract.left.isEmpty) None
            else {
              val x = zipper.map(_.unsafeMoveLeft)
              (x, x).some
            }
          )
        val duplicateRights: LazyList[Zipper[Zipper[A]]] =
          LazyList.unfold(s)(zipper =>
            if (zipper.extract.right.isEmpty) None
            else {
              val x = zipper.map(_.unsafeMoveRight)
              (x, x).some
            }
          )
        Zipper(duplicateLefts, s, duplicateRights)
      }
    }
  }

  implicit val gridZipperUnorderedFoldableWithInitValue: UnorderedFoldableWithInitValue[GridZipper] =
    new UnorderedFoldableWithInitValue[GridZipper] {
      override def unorderedFold[A, B](gridZipper: GridZipper[A])(z: B)(f: (B, A) => B): B = {
        val leftFolded = gridZipper.value.left.foldLeft(z)((acc, zipper) => zipper.unorderedFold(acc)(f))
        val focusFolded = gridZipper.value.focus.unorderedFold(leftFolded)(f)
        gridZipper.value.right.foldLeft(focusFolded)((acc, zipper) => zipper.unorderedFold(acc)(f))
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
