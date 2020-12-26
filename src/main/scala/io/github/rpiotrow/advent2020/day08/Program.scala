package io.github.rpiotrow.advent2020.day08

case class InstructionIndex(value: Int) extends AnyVal {
  def next = InstructionIndex(value + 1)
  def change(delta: Int) = InstructionIndex(value + delta)
}

case class Accumulator(value: Int) extends AnyVal {
  def change(delta: Int) = Accumulator(value + delta)
}

case class Program(value: Array[Instruction]) {
  def instruction(index: InstructionIndex): Instruction = value(index.value)
  def isTerminated(state: ConsoleState): Boolean = state.instructionIndex.value == value.length
}
