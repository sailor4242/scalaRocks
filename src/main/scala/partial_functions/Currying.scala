package partial_functions

object Currying extends App {

  val adder: Int => Int => Int = x => y => x + y

  val adder3 = adder(3)

  println(adder3(5))

  def curriedDef(x: Int)(y: Int): Int = x + y

  val defAdder3: Int => Int = curriedDef(3) // return type is needed

  // lifting = ETA-EXPANSION
  def inc(x: Int) = x + 1

  List(1,2,3).map(x => inc(x)) // ETA

  val add5 = curriedDef(5) _ // Int => Int

  // EXERCISE

  val addFunc = (x: Int, y: Int) => x + y
  def addDef(x: Int, y: Int) = x + y
  def curDef(x: Int)(y: Int) = x + y

  //add7
  val add7 = (x: Int) => addFunc(7, x)
  val add7_2 = addFunc.curried(7)
  val add7_3 = curDef(7)_ //PAF
  val add7_4 = curDef(7)(_) //PAF
  val add7_5 = addDef(7, _: Int) // forces Eta expansion, turning method into func value
  val add7_6 = addFunc(7, _: Int)

  //underscores are powerfull
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello", _: String, "123")
  println(insertName("Dima"))


  //EXERCISE
  println("%8.4f".format(Math.PI))
  def formatter(arg : Any)(s: String) = s.format(arg)
  val piFormatter = formatter(Math.PI)_
  println(List("%4.2f", "%8.6f", "%14.12f").map(piFormatter))

  /*
  - func vs methods
  - parameters: by-name vs 0-lambda
  calling by name and by func
   */
  def byName(n: => Int) = n + 1
  def byFunc(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23) // ok
  byName(method) // ok
  byName(parenMethod()) // ok
  byName(parenMethod) // ok but beware ==> byName(parenMethod())
  //byName(() => 42) //not ok
  byName((() => 42)()) // ok
  //byName(parenMethod _) // not ok

  // byFunc(45) // not ok
  // byFunc(method) // not ok !! compiler does not do eta
  byFunc(parenMethod) // ok
  byFunc(() => 45) // ok
  byFunc(parenMethod _) // also work, but warning -unnecessary
}
