package concurrency

object Concur extends App {

  // Exercise 1
  def inception(n: Int): Unit = {
    if (n < 50) {
      val th = new Thread(() => {
        inception(n + 1)
      })
      th.start()
      th.join()
    }
    println(s"I am thread $n")
  }

  //inception(0)

  // Exercise 2
  // producer-consumer

  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0
    def set(newValue: Int): Unit = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons() : Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      while (container.isEmpty) {
        println("[consumer] actively waiting")
      }
      println(s"[comsumer] i have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] i have produced $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }
  naiveProdCons()

}
