package partial_functions

object Monads extends App {

  // Try monad
  trait Attemp[+A] {
    def flatMap[B](f: A => Attemp[B]): Attemp[B]
  }

  object Attempt {
    def apply[A](a: => A): Attemp[A] = {
      try {
        Success(a)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  case class Success[+A](v: A) extends Attemp[A] {
    override def flatMap[B](f: A => Attemp[B]): Attemp[B] =
      try {
        f(v)
      } catch {
        case e: Throwable => Failure(e)
      }
  }
  case class Failure(e: Throwable) extends Attemp[Nothing] {
    override def flatMap[B](f: Nothing => Attemp[B]): Attemp[B] = this
  }

  /*
    left-identity

    unit.flat(f) = f(x)
    Attemo(x).flatMap(f) = f(x) // Success case!
    Success(x).flatMap(f) = f(x) // prooved

    right-identity

    attempt.flatMap(unit) = attempt
    Success(x).flatMap(x => Attemp(x)) = Attmept(x) = Success(x)
    Fail.flatMap(...) = Failure(e)

    associativity

    attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
    Fail(e).flatMap(f).flatMap(g) = Fail(e)
    Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

    Success(v).flatMap(f).flatMap(g) = f(v).flatMap(g) OR Fail(g)
   */

  //Exercise
  // implement lazy monad
  class Lazy[+A](v: => A) {
    private lazy val internalV = v
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(v)
    def use: A = internalV
  }
  object Lazy {
    def apply[A](a: => A): Lazy[A] = new Lazy(a)
  }

  val l = Lazy({println("1234")})
   l.use
   l.use
  val ll = l.flatMap(x => Lazy({println("321")}))
  // ll.use

  //Exercise
  trait Monad[T] {
     def apply[T](t: => T): Monad[T] = ???
     def flatMap[B](f: T => Monad[B]): Monad[B] = ???
     def map[B](f: T => B): Monad[B] = flatMap(x => apply(f(x)))
     def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap(x => x)
  }
}
