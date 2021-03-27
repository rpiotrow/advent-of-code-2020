package io.github.rpiotrow.advent2020.day13

case class Bus(id: Long) extends AnyVal {
  def earliestDepartureInMinutes(now: Timestamp): Long = {
    val modulo = now.value % id
    if (modulo == 0L)
      0L
    else
      id - modulo
  }
}
