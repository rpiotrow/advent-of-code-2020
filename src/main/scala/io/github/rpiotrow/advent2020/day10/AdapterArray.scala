package io.github.rpiotrow.advent2020.day10

import io.github.rpiotrow.advent2020.{Input, Solution}
import zio._

object AdapterArray {

  val solution: Solution =
    for {
      list                <- Input.readNumbers("day10.input")
      adapters            =  Adapters(list)
      joltsDiffs          =  new AdapterJoltsDiffs(adapters)
      joltDiffsResult     <- joltsDiffs.diff
      joltDiffsMultiplied =  joltDiffsResult.diff1Count * joltDiffsResult.diff3Count
      variants            =  new AdaptersVariants(adapters)
      variantsCount       <- variants.count
      _ <- console.putStrLn(s"Number of 1-jolt differences multiplied by the number of 3-jolt differences is $joltDiffsMultiplied")
      _ <- console.putStrLn(s"The total number of distinct ways you can arrange the adapters is $variantsCount")
    } yield (joltDiffsMultiplied, variantsCount)

}
