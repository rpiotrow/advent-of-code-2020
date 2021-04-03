package io.github.rpiotrow.advent2020.day13

import cats.data.NonEmptyList
import zio.{IO, ZIO}
import zio.stream.ZStream

case class ContestBusRequirement(busId: Long, offset: Long)

object ContestBusRequirement {
  def parseList(list: String): IO[String, NonEmptyList[ContestBusRequirement]] =
    ZStream
      .fromIterable(list.split(','))
      .zip(ZStream.iterate(0L)(_ + 1L))
      .mapM {
        case ("x", _) => ZIO.none
        case (idStr, offset) =>
          for {
            id <- ZIO.effect(idStr.toLong).mapError(_.getMessage)
          } yield Some(ContestBusRequirement(id, offset))
      }
      .collectSome
      .runCollect
      .map(c => NonEmptyList.fromList(c.toList))
      .flatMap(ZIO.fromOption(_).orElseFail("invalid input"))
}
