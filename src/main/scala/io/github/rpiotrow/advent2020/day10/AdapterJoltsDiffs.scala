package io.github.rpiotrow.advent2020.day10

import zio.{IO, ZIO}

class AdapterJoltsDiffs(private val adapters: Adapters) {

  import AdapterJoltsDiffs._

  def diff: IO[String, JoltsDiff] = {
    ZIO.foldLeft(adapters.list)(JoltsDiffAcc.zero)((acc, n) => {
      Math.abs(acc.previous - n) match {
        case 1 => ZIO.succeed(acc.copy(previous = n, diff1Count = acc.diff1Count + 1))
        case 2 => ZIO.succeed(acc.copy(previous = n, diff2Count = acc.diff2Count + 1))
        case 3 => ZIO.succeed(acc.copy(previous = n, diff3Count = acc.diff3Count + 1))
        case r => ZIO.fail(s"no solution found for ${acc.previous} $n $r")
      }
    }).map(acc => JoltsDiff(acc.diff1Count, acc.diff2Count, acc.diff3Count + 1))
  }
}

object AdapterJoltsDiffs {
  case class JoltsDiff(
    diff1Count: Int,
    diff2Count: Int,
    diff3Count: Int,
  )

  case class JoltsDiffAcc(
    previous: Long,
    diff1Count: Int,
    diff2Count: Int,
    diff3Count: Int,
  )
  object JoltsDiffAcc {
    val zero = JoltsDiffAcc(0L, 0, 0, 0)
  }
}
