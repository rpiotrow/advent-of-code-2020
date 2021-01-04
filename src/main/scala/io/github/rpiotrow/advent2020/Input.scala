package io.github.rpiotrow.advent2020

import zio.ZIO
import zio.blocking.Blocking
import zio.stream._

import java.io.IOException

object Input {
  def readStrings(inputFileName: String): ZStream[Blocking, String, String] = {
    val inputStream = Option(this.getClass.getClassLoader.getResourceAsStream(inputFileName))
    val zioInputStream = ZIO.fromOption(inputStream).orElseFail(new IOException("not found"))
    ZStream.fromInputStreamEffect(zioInputStream)
      .transduce(Transducer.utf8Decode)
      .mapError(_.getMessage)
  }

  def readLines(inputFileName: String): ZStream[Blocking, String, String] =
    readStrings(inputFileName).transduce(ZTransducer.splitLines)

  def readNumbers(inputFileName: String): ZIO[Blocking, String, List[Long]] =
    readLines(inputFileName)
      .mapM(line => ZIO.effect(line.toLong).orElseFail(s"invalid number: $line"))
      .runCollect
      .map(_.toList)
}
