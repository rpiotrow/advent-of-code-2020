package io.github.rpiotrow.advent2020.day10

import zio.{IO, ZIO}

class AdaptersVariants(private val adapters: Adapters) {

  import AdaptersVariants.Counter

  def count: IO[String, Long] = {
    val extendedList = (List(0L) ++ adapters.list ++ List(adapters.list.last + 3L))
    ZIO.foldRight(extendedList)(List.empty[Counter]) { (number, acc) =>
      acc match {
        // first adapter on current acc cannot be skipped
        case l@Counter(c1, _) :: Counter(_, n2) :: _ if (n2 - number) > 3 =>
          ZIO.succeed(Counter(c1, number) :: l)
        // only first adapter on current acc can be skipped
        case l@Counter(c1, _) :: Counter(c2, _) :: Counter(_, n3) :: _ if (n3 - number) > 3 =>
          ZIO.succeed(Counter(c1 + c2, number) :: l)
        // two first adapters on current acc can be skipped
        case l@Counter(c1, _) :: Counter(c2, _) :: Counter(c3, n3) :: _ if (n3 - number) == 3 =>
          ZIO.succeed(Counter(c1 + c2 + c3, number) :: l)
        // only two and we know (from the first case) that first adapter can be skipped
        case l@Counter(c1, _) :: Counter(c2, _) :: Nil =>
          ZIO.succeed(Counter(c1 + c2, number) :: l)
        // between last and wall it is always difference of 3
        case Counter(1, n2) :: Nil =>
          ZIO.succeed(Counter(1, number) :: Counter(1, n2) :: Nil)
        case Nil =>
          ZIO.succeed(Counter(1L, number) :: Nil)
        case s => ZIO.fail("invalid input")
      }
    }.map(_.head.count)
  }
}

object AdaptersVariants {
  case class Counter(count: Long, number: Long)
}
