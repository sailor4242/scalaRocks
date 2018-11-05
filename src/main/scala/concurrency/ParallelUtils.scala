package concurrency

import java.util.concurrent.atomic.AtomicReference

import scala.collection.parallel.immutable.ParVector

object ParallelUtils extends App {

  // parallel collections
  val parList = List(1,2,3).par
  val parVector = ParVector[Int](1,2,3)

  def measure[T](op: => T): Long = {
    val time = System.currentTimeMillis()
    op
    System.currentTimeMillis() - time
  }

  val list = (1 to 100000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
  val parallelTime = measure {
    list.par.map(_ + 1)
  }

  println(s"serial time $serialTime")
  println(s"parallel time $parallelTime")

  // atomic ops and references
  val atomic = new AtomicReference[Int](2)
  val currentValue = atomic.get() // thread-safe read
  atomic.set(4) // thread-safe write

  atomic.updateAndGet(_ + 1) // thread-safe function run
  atomic.getAndUpdate(_ + 1) // thread-safe function run

  atomic.accumulateAndGet(12, _ + _) // accumulation

}
