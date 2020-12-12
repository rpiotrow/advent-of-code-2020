package io.github.rpiotrow

import zio.{ZEnv, ZIO}

package object advent2020 {

  type Solution = ZIO[ZEnv, String, Unit]

}
