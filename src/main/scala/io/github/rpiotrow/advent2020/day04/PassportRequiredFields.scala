package io.github.rpiotrow.advent2020.day04

case class PassportRequiredFields(
  birthYear: String,
  issueYear: String,
  expirationYear: String,
  height: String,
  hairColor: String,
  eyeColor: String,
  passportID: String,
) {
  def areValid: Boolean =
    isBirthYearValid &&
      isIssueYearValid &&
      isExpirationYearValid &&
      isHeightValid &&
      isHairColorValid &&
      isEyeColorValid &&
      isPassportIDValid

  import PassportRequiredFields._

  private def isBirthYearValid = yearBetween(birthYear, 1920, 2002)
  private def isIssueYearValid = yearBetween(issueYear, 2010, 2020)
  private def isExpirationYearValid = yearBetween(expirationYear, 2020, 2030)
  private def isHeightValid =
    height match {
      case heightPattern(valueString, unit) => {
        val value = valueString.toInt
        if (unit == "cm")
          value >= 150 && value <= 193
        else
          value >= 59 && value <= 76
      }
      case _ => false
    }
  private def isHairColorValid = hairColorPattern.matches(hairColor)
  private def isEyeColorValid = eyeColors.contains(eyeColor)
  private def isPassportIDValid = passportIDPattern.matches(passportID)
}

object PassportRequiredFields {
  private val yearPattern = raw"(\d{4})".r
  private val heightPattern = raw"(\d{2,3})(cm|in)".r
  private val hairColorPattern = raw"#[0-9a-f]{6}".r
  private val eyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  private val passportIDPattern = raw"(\d{9})".r

  private def yearBetween(value: String, from: Int, to: Int) =
    value match {
      case yearPattern(yearString) => {
        val year = yearString.toInt
        year >= from && year <= to
      }
      case _ => false
    }
}