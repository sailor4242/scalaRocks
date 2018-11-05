package partial_functions

import scala.annotation.tailrec

//EXERCISE
// Implement a lazily evaluated, singly linked Stream of elements
// Examples :
/*
 naturals = MyStream.from(1)(x => x + 1) // stream of natural numbers (potentially infinite)
 naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite)
 naturals.foreach(println) // will crash - infinite!
 naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
*/
abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](elem: B): MyStream[B] // prepend operator
  def ++[B >: A](stream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(p: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements of thisstream
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil) : List[B] = {
    if (isEmpty) acc
    else tail.toList(head :: acc)
  }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    new Cons(start, from(generator(start))(generator))
  }
}

class EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](elem: B): MyStream[B] = new Cons[B](elem, this)
  override def ++[B >: Nothing](stream: => MyStream[B]): MyStream[B] = stream

  override def foreach(f: Nothing => Unit): Unit = ()
  override def map[B](f: Nothing => B): MyStream[B] = this
  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  override def filter(p: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this
}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false
  override def head: A = hd
  override def tail: MyStream[A] = tl

  override def #::[B >: A](elem: B): MyStream[B] = new Cons[B](elem, this)
  override def ++[B >: A](stream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ stream)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  override def filter(p: A => Boolean): MyStream[A] = {
    if (p(head)) new Cons(head, tail.filter(p))
    else tail.filter(p)
  }

  override def take(n: Int): MyStream[A] = {
    if (n < 1) new EmptyStream
    else new Cons(head, tail.take(n - 1))
  }
}

object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)
  val startFrom0 = 0 #:: naturals
  println(startFrom0.map(_ * 2).take(10).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, new EmptyStream))).take(10).toList())
  println(startFrom0.filter(_ < 10).take(9).toList())

  //Exercises
  // 1 -stream of Fibonacci numbers
  // 2- stream of prime numbers with Eratosthenes' sieve
  //    [2 3] 4 .. ]
  //     filter out all numbers divisible by 2 - [2 3 5 7 9] - filter out all numbers divisible by 3 .. etc

  def fibonachi(x: Int, y: Int) : MyStream[Int] = new Cons(x, fibonachi(y, x + y))
  println(fibonachi(1, 1).take(10).toList())

  val mod: (Int, Int) => Boolean = (x: Int, y: Int) => x % y != 0

  val erathos = MyStream.from(2)(_ + 1)
    .filter(mod(_,2))
    .filter(mod(_,3))
    .filter(mod(_,5))
    .filter(mod(_,7)).take(50).toList()
  println(erathos)
}
