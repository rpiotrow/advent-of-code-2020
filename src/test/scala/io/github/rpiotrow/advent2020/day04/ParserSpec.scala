package io.github.rpiotrow.advent2020.day04

import zio.test.Assertion.equalTo
import zio.test._

import cats.implicits._

object ParserSpec extends DefaultRunnableSpec {
  def spec = suite("day 04: ParserSpec")(
    testM("parse passport data") {
      val string = """eyr:2029 ecl:blu cid:129 byr:1989
                     |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm""".stripMargin
      val expected = PassportData(
        birthYear = "1989".some,
        issueYear = "2014".some,
        expirationYear = "2029".some,
        height = "165cm".some,
        hairColor = "#a97842".some,
        eyeColor = "blu".some,
        passportID = "896056539".some,
        countryID = "129".some)
      for {
        passportData <- Parser.parsePassport(string)
      } yield assert(passportData)(equalTo(expected))
    }
  )
}

