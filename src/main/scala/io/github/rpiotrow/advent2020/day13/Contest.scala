package io.github.rpiotrow.advent2020.day13

object Contest {

  private case class Acc(minute: Long, period: Long)

  def compute(firstBus: Long, requirements: List[ContestBusRequirement]): Long =
    requirements.foldLeft(Acc(0, firstBus)) { (acc, requirement) =>
      //acc.minute + x*acc.period (mod requirement.busId) ≡ (requirement.busId - requirement.offset)
      val b = (requirement.busId - requirement.offset) - acc.minute
      val modularEquation = ModularEquation(acc.period, requirement.busId, b % requirement.busId)
      val newPeriod = acc.period * requirement.busId
      Acc(
        (acc.minute + (modularEquation.x * acc.period)) % newPeriod,
        newPeriod
      )
    }.minute

}
