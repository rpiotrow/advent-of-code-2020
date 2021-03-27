package io.github.rpiotrow.advent2020.day13

case class BusDeparture(bus: Bus, minutesToWait: Long) {
  def multiplication: Long = bus.id * minutesToWait
}

object BusDeparture {
  implicit val ordering: Ordering[BusDeparture] =
    Ordering[Long].on[BusDeparture](_.minutesToWait)
}