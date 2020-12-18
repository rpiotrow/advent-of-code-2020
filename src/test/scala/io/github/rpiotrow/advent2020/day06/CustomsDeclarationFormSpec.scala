package io.github.rpiotrow.advent2020.day06

import zio.test.Assertion.equalTo
import zio.test._

object CustomsDeclarationFormSpec extends DefaultRunnableSpec {
  def spec = suite("day 06: CustomsDeclarationFormSpec")(
    testM("parse customs form any ab ac") {
      val string =
        """ab
          |ac""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAny(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('a', 'b', 'c'))))
    },
    testM("parse customs form any a b c") {
      val string =
        """a
          |b
          |c""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAny(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('a', 'b', 'c'))))
    },
    testM("parse customs form any a a a a") {
      val string =
        """a
          |a
          |a
          |a""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAny(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('a'))))
    },
    testM("parse customs form any b") {
      val string =
        """b""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAny(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('b'))))
    },
    testM("parse customs form all ab ac") {
      val string =
        """ab
          |ac""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAll(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('a'))))
    },
    testM("parse customs form all a b c") {
      val string =
        """a
          |b
          |c""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAll(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set())))
    },
    testM("parse customs form all a a a a") {
      val string =
        """a
          |a
          |a
          |a""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAll(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('a'))))
    },
    testM("parse customs form all b") {
      val string =
        """b""".stripMargin
      for {
        result <- CustomsDeclarationForm.fromStringWhenAll(string)
      } yield assert(result)(equalTo(CustomsDeclarationForm(Set('b'))))
    }
  )
}

