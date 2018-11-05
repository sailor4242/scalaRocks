package implicits

object PimpMyLibrary extends App {

  //2.isPrime

  implicit class RichInt(val value: Int) {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times(f: () => Unit): Unit = {
      def aux(n: Int): Unit = {
        if (n <= 0)()
        else
          f()
          aux(n - 1)
      }

      aux(value)
    }
  }

  implicit class RicherInt(r: RichInt) {
    def isOdd: Boolean = r.value % 2 != 0
  }

  new RichInt(42).sqrt

  42.isEven // type inrichment = pimping = new RichInt(42).isEven

  1 to 10

  // compile doesn't do multiple implicit searches
  // 42.isOdd

  /*
  Exercise:
   */

  implicit class StringExercise(val s: String) {
    def asInt: Integer = s.toInt
    def encrypt(cypher: Int): String = s.map(c => (c + cypher).asInstanceOf[Char])
  }

  println("123".asInt)
  println("123".encrypt(2))

 // implicit def stringToInt(s: String): Int = s.toInt // avoid implicit defs
  // println("6" / 2)
}
