package io.github.rpiotrow

import zio.ZIO
import zio.blocking.Blocking
import zio.console.Console

package object advent2020 {

  type Solution = ZIO[Console with Blocking, Serializable, Unit]

}
