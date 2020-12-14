package io.github.rpiotrow.advent2020.day04

import zio.{IO, ZIO}

object Parser {
  def parsePassport(string: String): IO[String, PassportData] = {
    parseFields(string.replace('\n', ' ').split(" ")).map(fields =>
      PassportData(
        birthYear = fields.get("byr"),
        issueYear = fields.get("iyr"),
        expirationYear = fields.get("eyr"),
        height = fields.get("hgt"),
        hairColor = fields.get("hcl"),
        eyeColor = fields.get("ecl"),
        passportID = fields.get("pid"),
        countryID = fields.get("cid")
      )
    )
  }

  private def parseFields(fields: Array[String]): IO[String, Map[String, String]] =
    ZIO.collectAll(fields.map(parseField)).map(_.toMap)

  private def parseField(field: String) =
    ZIO.succeed(field.split(":"))
      .filterOrFail(_.length == 2)("parse error")
      .map(arr => arr(0) -> arr(1))
}
