package partial_functions

object Lazy extends App {

  // lazy delays the evaluation
  lazy val x: Int = throw new RuntimeException

  lazy val y : Int = {
    println("hello")
    42
  }
  println(y)
  println(y)

  //examples of implications

  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false
  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  //in conjunction with call by name

  def byName(n: => Int): Int = n + n + n + 1
  def retrieveMagic = {
    //side effect or ong computation
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byName(retrieveMagic))

  // use lazy vals
  def byNameLazy(n: => Int): Int = {
    lazy val t = n // only evaluated once
    t + t + t + 1
  }
  println(byNameLazy(retrieveMagic)) // call by NEED

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30) // List(1, 25, 5, 23)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println("------")
  println(gt20lazy)
  println("------")
  gt20lazy.foreach(println)

  //for-comprehensions use withFilter with guards

  for {
    a <- List(1,2,3) if a % 2 == 0 // use lazy vals!
  } yield a + 1
  // List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1)

}
