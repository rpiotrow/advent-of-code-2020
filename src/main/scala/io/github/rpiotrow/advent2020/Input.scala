package io.github.rpiotrow.advent2020

import zio.ZIO
import zio.blocking.Blocking
import zio.stream.{Transducer, ZStream, ZTransducer}

import java.io.IOException

object Input {
  def readLines(inputFileName: String): ZStream[Blocking, String, String] = {
    val inputStream = Option(this.getClass.getClassLoader.getResourceAsStream(inputFileName))
    val zioInputStream = ZIO.fromOption(inputStream).orElseFail(new IOException("not found"))
    ZStream.fromInputStreamEffect(zioInputStream)
      .transduce(Transducer.utf8Decode)
      .transduce(ZTransducer.splitLines)
      .mapError(_.getMessage)
  }
}
