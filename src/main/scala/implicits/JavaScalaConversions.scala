package implicits
import java.{util => ju}

object JavaScalaConversions  extends App {

  import collection.JavaConverters._

  val javaSet: ju.Set[Int] = new ju.HashSet[Int]()
  (1 to 5).foreach(javaSet.add)
  println(javaSet)

  val scalaSet = javaSet.asScala // !! Mutable set

  import collection.mutable._

  val buffer = ArrayBuffer[Int](1, 2, 3)
  val javaArray = buffer.asJava // to Java

  /*
  Exercise implement conversion from Java Optional to scala Option
   */
  class ToScala[T](value: => T) {
    def asScala: T = value
  }

  implicit def asScalaOptional[A](o : ju.Optional[A]): ToScala[Option[A]] = new ToScala[Option[A]](
    if (o.isPresent) Some(o.get()) else None
  )

  val javaOptional = ju.Optional.of(3).asScala
  val javaOptionalEmpty = ju.Optional.ofNullable(null).asScala
}

