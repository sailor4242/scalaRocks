package partial_functions

object PF extends App {

  val func = (x: Int) => x + 1

  val aNicerFunc = (x: Int) => x match {
    case 1 => 42
    case 2 => 55
  }

  val aPartioalFunc: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 55
  }

  aPartioalFunc(2)
  aPartioalFunc.isDefinedAt(3)

  val lifted = aPartioalFunc.lift // Int => Option[Int]
  lifted(2)
  lifted(3)

  val pfChain = aPartioalFunc.orElse[Int, Int] {
    case 3 => 11
    case 4 => 99
  }

  pfChain(2)
  pfChain(3)

//  val aMappedListOne = List(1,2,3).map {
//    aPartioalFunc(_)
//  }

  val aMappedListTwo = List(1,2,3).map {
    lifted(_)
  }

  print(aMappedListTwo)


 // scala.io.Source.stdin.getLines().foreach(line => println("You said" + line))

  val myPF = new PartialFunction[String, String] {

    override def apply(s: String): String = s match {
      case s => "You said " + s
    }

    override def isDefinedAt(s: String): Boolean = {
      !s.isEmpty
    }

  }

  scala.io.Source.stdin.getLines().map(myPF.lift).foreach(println)
}
