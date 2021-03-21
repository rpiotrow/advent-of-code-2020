package io.github.rpiotrow.advent2020.day11.zzippers
import zio.stream.ZStream
import zio._

import cats.implicits._

object ATest extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    (for {
      z <- ZZipper.fromList(List(1,2,3,4,5))
      zz = z.coflatMap(e => e.extract)
      zl1 <- zz.moveLeft
      zl2 <- zz.moveLeft
      zl3 <- zz.moveLeft
      zl4 <- zz.moveLeft
      zl5 <- zz.moveLeft
      zl6 <- zz.moveLeft
      zl7 <- zz.moveLeft
      _ <- zio.console.putStrLn(":D")
//      l <- z.left.runCollect.map(_.toList)
//      _ <- zio.console.putStrLn(l.toString)
//      z1 <- z.moveLeft
//      l1 <- z1.left.runCollect.map(_.toList)
//      _ <- zio.console.putStrLn(l1.toString)
//      z2 <- z.moveLeft
//      l2 <- z2.left.runCollect.map(_.toList)
//      _ <- zio.console.putStrLn(l2.toString)
//      _ <- z1.left.foreach(i => zio.console.putStrLn(s"left $i")).run
//      _ <- zio.console.putStrLn(z1.focus.toString)
//      _ <- z1.right.foreach(i => zio.console.putStrLn(s"right $i")).run
//      z2 <- z1.moveLeft
//      _ <- zio.console.putStrLn(z2.focus.toString)
    } yield ()).exitCode
  }
}
