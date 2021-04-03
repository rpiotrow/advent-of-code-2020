package io.github.rpiotrow.advent2020.day14

case class ComputerSystem(memory: Map[Int, Long], mask: Mask) {
  def process(instruction: Instruction): ComputerSystem =
    instruction match {
      case SetMask(mask) => copy(mask = mask)
      case WriteToMemory(address, value) => copy(memory.updated(address, mask.apply(value)))
    }
  def sum: Long = memory.values.sum
}

object ComputerSystem {
  val zero: ComputerSystem = ComputerSystem(Map(), Mask(0, ~0))
}
