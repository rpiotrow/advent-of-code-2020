package io.github.rpiotrow.advent2020.day02

trait PasswordPolicy {
  def isPasswordValid(password: Password): Boolean
}

case class FirstPasswordPolicy(
  minOccurrences: Int,
  maxOccurrences: Int,
  requiredLetter: Char
) extends PasswordPolicy {

  override def isPasswordValid(password: Password): Boolean = {
    val count = password.value.toList.count(_ == requiredLetter)
    count >= minOccurrences && count <= maxOccurrences
  }

  def toSecondPolicy = SecondPasswordPolicy(
    index1 = minOccurrences,
    index2 = maxOccurrences,
    letter = requiredLetter
  )
}

case class SecondPasswordPolicy(
  index1: Int,
  index2: Int,
  letter: Char
) extends PasswordPolicy{

  override def isPasswordValid(password: Password): Boolean = {
    val charList = password.value.toList
    val letter1 = charList.length >= index1 && charList(index1-1) == letter
    val letter2 = charList.length >= index2 && charList(index2-1) == letter
    (letter1 || letter2) && (letter1 != letter2)
  }
}
