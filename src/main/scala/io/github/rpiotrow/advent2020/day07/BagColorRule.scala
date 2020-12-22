package io.github.rpiotrow.advent2020.day07

import zio._

case class BagColorRule(outer: BagColor, inner: List[(Amount, BagColor)]) {
  def pair: (BagColor, List[(Amount, BagColor)]) = (outer, inner)
  def pairsReversed: List[(BagColor, BagColor)] =
    inner.toList.map { case (_, color) => (color, outer)}
}

object BagColorRule {
  private val outerPattern = raw"(\w+ \w+) bags contain (.*)".r
  private val innerPattern = raw"(\d+) (\w+ \w+) bag[s]?[.]?".r

  def fromString(string: String): IO[String, BagColorRule] = {
    string match {
      case outerPattern(outerColorString, innerContent) =>
        val outerBagColor = BagColor(outerColorString)
        for {
          innerBagColors <- innerBags(innerContent)
        } yield BagColorRule(outerBagColor, innerBagColors)
      case _ => ZIO.fail("parse error of outer pattern")
    }
  }

  private def innerBags(innerContent: String): IO[String, List[(Amount, BagColor)]] = {
    if (innerContent == "no other bags.")
      ZIO.succeed(List())
    else {
      ZIO.collectAll(innerContent.split(", ").map(innerBag)).map(_.toList)
    }
  }

  private def innerBag(innerContent: String): IO[String, (Amount, BagColor)] = {
    innerContent match {
      case innerPattern(amountString, innerColorString) =>
        ZIO.succeed((Amount(amountString.toInt), BagColor(innerColorString)))
      case _ => ZIO.fail(s"parse error of inner pattern '$innerContent'")
    }
  }
}
