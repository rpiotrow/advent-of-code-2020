package io.github.rpiotrow.advent2020.day13

//  gcd(a, b) = a*x + b*y
sealed abstract class ExtendedGCD(val a: Long, val b: Long) {
  def gcd: Long
  def x: Long
  def y: Long
}

object ExtendedGCD {

  def apply(a: Long, b: Long): ExtendedGCD =
    if (a == 0)
      new ExtendedGCD(a, b) {
        val x = 0
        val y = 1
        val gcd = b
      }
    else {
      val egcd = apply(b%a, a)
      new ExtendedGCD(a, b) {
        def x: Long = egcd.y - (b/a) * egcd.x
        def y: Long = egcd.x
        def gcd: Long = egcd.gcd
      }
    }

}
