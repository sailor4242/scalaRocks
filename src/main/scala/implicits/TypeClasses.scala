package implicits

object TypeClasses extends App {

  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int = 0, email: String = "") extends HTMLWritable {
    override def toHtml: String = s"<dev>$name ($age yo) <a href=$email/> </div>"
  }

  // 1
  User("jhon", 32, "123").toHtml
  // cons: works for the types WE write, ONE impl out of quite a number

  // 2
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value  match {
      case User(n, a , e) =>
      // case java.util.Date =>
      case _ =>
    }
  }
  // cons: lost type safety, need to modify the code every time, still ONE implementation

  // 3
  trait HTMLSerializer[T] { // - type class
    def serialize(value: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] { // - type class instances
    override def serialize(value: User): String = "123"
  }
  object PartialUserSerialiazer extends HTMLSerializer[User] {
    override def serialize(value: User): String = "321"
  }
  // prons, cons: we can define serializers for other types, we can define multiple serializers for every type




  //PART 2

  object  HTMLSerializer {
    def serialize[T](t: T)(implicit serializer:  HTMLSerializer[T]): String =
      serializer.serialize(t)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object IntSerializer extends  HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"$value - 1234"
  }

  //println(HTMLSerializer.serialize(42)(IntSerializer))
  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(User("jhon", 32, "123")))
  println(HTMLSerializer[User].serialize(User("jhon", 32, "1234")))

  /*
    Exercise: implement the TC pattern for the Equality tt
   */
  trait Equal[A] {
    def equal(a: A, b: A) : Boolean
  }

  implicit object UserEqualizer extends Equal[User] {
    override def equal(a: User, b: User): Boolean = a.name == b.name
  }

  object Equal {
    def apply[A](a: A, b: A)(implicit equalizer: Equal[A]) = equalizer.equal(a, b)
  }
  val jhon1 = User("jhon")
  val jhon2 = User("jhon2")
  println(Equal(jhon1, jhon2)) // AD-HOC polymorphism !!!



  //PART 3
  implicit class HTMLEnrichment[T](value: T) {
    def toHtml2(implicit serializer: HTMLSerializer[T]) = serializer.serialize(value)
    // "2" because user already has the same method
  }
  println(jhon1.toHtml2(UserSerializer)) //println(new HTMLEnrichment[User](jhon).toHtml(UserSerializer)
  println(jhon1.toHtml2(PartialUserSerialiazer)) //println(new HTMLEnrichment[User](jhon).toHtml(UserSerializer)
  println(jhon1.toHtml2)
  println(2.toHtml2)
  // extends functionality
  // expressive
  // type class itself

  /*
  Exercise implicit conversion for equals

   */
  implicit class EqualEnrichment[T](value: T) {
    def ===(aValue: T)(implicit equalizer: Equal[T]): Boolean = equalizer.equal(value, aValue)
    def !==(aValue: T)(implicit equalizer: Equal[T]): Boolean = !equalizer.equal(value, aValue)
  }

  // TYPE SAFE
  println(jhon1===jhon2)
  println(jhon1!==jhon1)
  println(jhon1 == 42) // not type safe
  // println(jhon1 === 42) // type safe

  // contex bounds
  def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]): String =
    s"sdvsdvcsd${content.toHtml2(serializer)}"

  def htmlSugar[T : HTMLSerializer](content: T): String = {
    // injects implicit serializer
    s"sdvsdvcsd${content.toHtml2}"
  }

  // implicitly
  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("ALL")

  val standartPerms = implicitly[Permissions]


  def htmlSugarImplicitly[T : HTMLSerializer](content: T): String = {
    // injects needen serializer
    val serializer = implicitly[HTMLSerializer[T]]
    s"sdvsdvcsd${content.toHtml2(serializer)}"
  }
}

