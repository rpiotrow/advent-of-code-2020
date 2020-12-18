package io.github.rpiotrow.advent2020.day06

import cats.{Monoid, Semigroup}
import cats.implicits._
import zio._

case class CustomsDeclarationForm(yesAnswers: Set[Char]) extends AnyVal

object CustomsDeclarationForm {
  def fromStringWhenAny(string: String): UIO[CustomsDeclarationForm] =
    ZIO.succeed(CustomsDeclarationForm(
      listOfCharSet(string).combineAll(monoidForAny)
    ))

  def fromStringWhenAll(string: String): IO[String, CustomsDeclarationForm] =
    ZIO.fromOption(
      for {
        nel <- listOfCharSet(string).toNel
      } yield nel.reduce(semigroupForAll)
    )
      .map(CustomsDeclarationForm(_))
      .orElseFail("empty form")

  private def listOfCharSet(string: String) =
    string
      .split('\n')
      .map(_.toSet)
      .toList

  private val monoidForAny = implicitly[Monoid[Set[Char]]]
  private val semigroupForAll = new Semigroup[Set[Char]] {
    override def combine(x: Set[Char], y: Set[Char]): Set[Char] = x intersect y
  }
}