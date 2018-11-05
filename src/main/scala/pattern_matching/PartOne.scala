package pattern_matching

object PartOne extends App {

  object X {
    def unapply(arg: Int, p: Int=>Boolean, s: String): Option[String] = {
      if (p(arg)) Some(s) else None
    }
  }

  val n: Int = 42

//  val mathProperty1 = n match {
//    case X(arg:Int, p:Boolean, s: String) if p(arg) => s"$s"
//  }

  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some(true) else None
  }

  val mathProperty2 = n match {
    case even(_) => ""
  }

  object isSingleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val mathProperty3 = n match {
    case isSingleDigit() => ""
  }

}
