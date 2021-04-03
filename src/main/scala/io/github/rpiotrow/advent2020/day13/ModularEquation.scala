package io.github.rpiotrow.advent2020.day13

// a*x (mod n) ≡ b
sealed abstract class ModularEquation(val a: Long, val n: Long, val b: Long) {
  def x: Long
}

object ModularEquation {
  def apply(a: Long, n: Long, b: Long): ModularEquation = {
    val eGcd = ExtendedGCD(a, n)
    if (eGcd.gcd == 1) {
      new ModularEquation(a, n, b) {
        val x = {
          val x1 = eGcd.x * b
          if (x1 < 0)
            x1 % n
          else
            x1
        }
      }
    } else
      throw new RuntimeException("Should not be needed for the ShuttleSearch")
  }
}
