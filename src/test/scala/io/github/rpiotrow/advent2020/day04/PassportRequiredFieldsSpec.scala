package io.github.rpiotrow.advent2020.day04

import zio.test.Assertion.equalTo
import zio.test._

object PassportRequiredFieldsSpec extends DefaultRunnableSpec {
  def spec = suite("PassportRequiredFields")(
    test("valid") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2030",
        height = "74in",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(true))
    },
    test("invalid birthYear value") {
      val fields = PassportRequiredFields(
        birthYear = "1900",
        issueYear = "2012",
        expirationYear = "2030",
        height = "74in",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid birthYear text") {
      val fields = PassportRequiredFields(
        birthYear = "do-not-remember",
        issueYear = "2012",
        expirationYear = "2030",
        height = "74in",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid issueYear") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "1985",
        expirationYear = "2030",
        height = "74in",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid expirationYear") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2015",
        height = "74in",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid height") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2030",
        height = "202cm",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid hairColor") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2030",
        height = "190cm",
        hairColor = "black",
        eyeColor = "grn",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid eyeColor") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2030",
        height = "190cm",
        hairColor = "#623a2f",
        eyeColor = "green",
        passportID = "087499704"
      )
      assert(fields.areValid)(equalTo(false))
    },
    test("invalid passportID") {
      val fields = PassportRequiredFields(
        birthYear = "1980",
        issueYear = "2012",
        expirationYear = "2030",
        height = "190cm",
        hairColor = "#623a2f",
        eyeColor = "grn",
        passportID = "666"
      )
      assert(fields.areValid)(equalTo(false))
    }
  )
}
