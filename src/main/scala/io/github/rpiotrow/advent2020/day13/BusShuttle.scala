package io.github.rpiotrow.advent2020.day13

import zio._
import zio.stream._

case class BusShuttle(now: Timestamp, buses: List[Bus]) {

  def earliestDeparture: BusDeparture =
    buses
      .map(bus => BusDeparture(bus, bus.earliestDepartureInMinutes(now)))
      .min
}

object BusShuttle {

  def parse(input: List[String]): IO[String, BusShuttle] =
    input match {
      case timestampString :: busListString :: Nil =>
        for {
          now <- parseLongValue(timestampString, Timestamp.apply)
          buses <- parseBusList(busListString)
        } yield BusShuttle(now, buses)
      case _ => IO.fail("invalid input")
    }

  private def parseBusList(string: String): IO[String, List[Bus]] =
    ZStream
      .fromIterable(string.split(','))
      .mapM {
        case "x" => ZIO.none
        case idStr => parseLongValue(idStr, Bus.apply).map(Some(_))
      }
      .collectSome
      .runCollect
      .map(_.toList)

  private def parseLongValue[A](string: String, apply: Long => A): IO[String, A] =
    for {
      value <- ZIO.effect(string.toLong).mapError(_.getMessage)
    } yield apply(value)
}