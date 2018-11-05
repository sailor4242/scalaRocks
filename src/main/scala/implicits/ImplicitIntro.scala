package implicits

object ImplicitIntro extends App {

  def inc(x: Int)(implicit amount: Int) = x + amount
  implicit val default = 10
  println(inc(2)) // not default args


  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_>_)
  //implicit val reverseOrdering(): Ordering[Int] = Ordering.fromLessThan(_>_)
  //implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_>_)
  println(List(1,2,3,4,5).sorted) // scala.Predf has implicit order

  /*
    Implicits (used as implicit parameters):
    - val/var
    - object
    - accessor methods = defs with no parentheses
   */

  //Exercise
  case class Person(name: String, age: Int)

  //implicit val alphabetOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name < p2.name)
  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("Jhon", 66)
  )
  //println(persons.sorted)

  /*
  Implicit scope by priority:
  - normal scope = LOCAL SCOPE
  - imported scope
  - companion objects of all types involved in the method signature
      // def sorted[B > A](implicit ord: Ordering[B]): Repr
      - List
      - Ordering
      - all the types involved = A or any supertype
   */

  object AlphabeticNameOrdering {
    implicit val alphabetOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name < p2.name)
  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.age < p2.age)
  }

  import AgeOrdering._
  println(persons.sorted)

  /*Exercise
    - totalPrice = mostUsed (50%)
    - by unit count (25%)
    - by unit price (25%)
  */
  case class Purchase(units: Int, unitPrice: Double)

  object CountOrdering {
    implicit val countOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.units < p2.units)
  }
  object PriceOrdering {
    implicit val priceOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.unitPrice < p2.unitPrice)
  }
  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) =>
      p1.unitPrice * p1.units < p2.unitPrice * p2.units)
  }
}
