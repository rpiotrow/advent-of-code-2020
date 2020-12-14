package io.github.rpiotrow.advent2020.day04

import cats.implicits._

case class PassportData(
  birthYear: Option[String],
  issueYear: Option[String],
  expirationYear: Option[String],
  height: Option[String],
  hairColor: Option[String],
  eyeColor: Option[String],
  passportID: Option[String],
  countryID: Option[String]
) {
  def requiredFields: Option[PassportRequiredFields] =
    (birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportID).mapN(PassportRequiredFields.apply)
}
