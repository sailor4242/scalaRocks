package typeSystem

object Inheritance extends App {


  // convenience
  trait Writer[T] {
    def write(t: T): Unit
  }

  trait Closeable {
    def close(status: Int): Unit
  }

  trait GenericStream[T] {
    def foreach(f: T => Unit): Unit
  }

  def processStream[T](stream: GenericStream[T] with Writer[T] with Closeable): Unit = {
    stream.foreach(println)
    stream.close(0)
  }

  // diamond problem
  trait Animal {
    def name: String
  }

  trait Lion extends Animal {
    override def name: String = "lion"
  }

  trait Tiger extends Animal {
    override def name: String = "tiger"
  }

  class Mutant extends Lion with Tiger {
    // last override get picked in diamond problem
  }

  val m = new Mutant
  println(m.name)

  // the super problem + type linearization
  trait Cold {
    def print = println("cold")
  }

  trait Green extends Cold {
    override def print: Unit =
      println("green")

    super.print
  }

  trait Blue extends Cold {
    override def print: Unit =
      println("blue")

    super.print
  }

  class Red {
    def print = println("red")
  }

  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("white")
      super.print
    }
  }

  val color = new White
  //color.print


  val s1 = "123"
  val s2 = "0"
  s1.intersect(s2).isEmpty

  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    a ++ a
  }

  def minimumBribes(q: Array[Int]) {
    var count = 0
    for (i <- 0 until q.length - 1) {
      val j = q(i)
      val diff = q(i) - i
      if (diff > 2) println("Too chaotic")
      else if (diff == 1 || diff == 2) count = count + 1
      else return
    }
    println(count)
  }

  minimumBribes(Array(1, 2, 5, 3, 4))
}
