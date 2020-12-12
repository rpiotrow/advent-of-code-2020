package io.github.rpiotrow

import zio.ZIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console

package object advent2020 {

  type Solution = ZIO[Console with Blocking with Clock, Serializable, Unit]

}
