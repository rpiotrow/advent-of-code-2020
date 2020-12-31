package io.github.rpiotrow.advent2020.day09

import zio.ZIO
import zio.stream.ZStream

import scala.collection.immutable.Queue

class Encoding(private val list: List[Long], private val bufferSize: Int) {

  def findFirstNotMatching: ZIO[Any, String, Long] = {
    def numberWithQueueOfPreviousNumbers(acc: (Option[Long], Queue[Long]), number: Long) = {
      val (previous, queue) = acc
      val enqueued = queue.enqueueAll(previous.toList)
      val newQueue = if (enqueued.size > bufferSize) {
        val (_, dequeued) = enqueued.dequeue
        dequeued
      } else {
        enqueued
      }
      ((Option(number), newQueue), (number, newQueue))
    }
    def isSumOfTwoFromSet(number: Long, set: Set[Long]): Boolean = {
      set.exists(e => e + e != number && set.contains(number - e))
    }
    ZStream
      .fromIterable(list)
      .mapAccum((Option.empty[Long], Queue.empty[Long]))(numberWithQueueOfPreviousNumbers)
      .map({ case (number, queue) => (number, queue.toSet)})
      .drop(bufferSize)
      .filterNot({ case (number, set) => isSumOfTwoFromSet(number, set)})
      .runHead
      .flatMap(ZIO.fromOption(_).orElseFail("solution not found"))
      .map({ case (number, _) => number })
  }

  def findContiguousRange(sumToFind: Long): ZIO[Any, String, (Long, Long)] = {
    def accumulateRange(acc: (Long, Queue[Long]), number: Long) = {
      val (sum, queue) = acc
      val newQueue = queue.enqueue(number)
      val newSum = sum + number
      if (newSum <= sumToFind)
        ((newSum, newQueue), (newSum, newQueue))
      else {
        val dequeued = dequeueUntil(newSum, newQueue)
        (dequeued, dequeued)
      }
    }
    def dequeueUntil(sum: Long, queue: Queue[Long]): (Long, Queue[Long]) = {
      if (sum <= sumToFind) {
        (sum, queue)
      } else {
        val (dequeuedNumber, newQueue) = queue.dequeue
        dequeueUntil(sum - dequeuedNumber, newQueue)
      }
    }
    ZStream
      .fromIterable(list)
      .mapAccum((0L, Queue.empty[Long]))(accumulateRange)
      .filter({case (sum, _) => sum == sumToFind})
      .map({case (_, queue) => {
        val list = queue.toList.sorted
        (list.head, list.last)
      }})
      .runHead
      .flatMap(ZIO.fromOption(_).orElseFail("solution not found"))
  }



}
