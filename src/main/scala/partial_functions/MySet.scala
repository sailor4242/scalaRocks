package partial_functions

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  override def apply(v1: A): Boolean = this.contains(v1)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(set: MySet[A]): MySet[A]

  def map[B](f: A=>B): MySet[B]
  def flatMap[B](f: A=>MySet[B]): MySet[B]
  def filter(p: A=> Boolean): MySet[A]
  def foreach(f: A=> Unit): Unit

  def -(elem: A): MySet[A]
  def &(set: MySet[A]): MySet[A] // intersection
  def --(set: MySet[A]): MySet[A] // difference

  def unary_! : MySet[A]


}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  override def ++(set: MySet[A]): MySet[A] = set
  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(p: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this
  override def &(set: MySet[A]): MySet[A] = this
  override def --(set: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)
  override def +(elem: A): MySet[A] = if (this contains elem) this else new NonEmptySet[A](elem, this)

  /*
   [1 2] ++ [3 4] =
   [2] ++ [3 4] + 1 =
   [] ++ [3 4] + 1 + 2 =
   [3 4] + 1 + 2 =
   [3 4 1 2]
   */
  override def ++(set: MySet[A]): MySet[A] = tail ++ set + head
  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)
  override def filter(p: A => Boolean): MySet[A] = {
    val filteredTail = tail filter p
    if (p(head)) filteredTail + head
    else filteredTail
  }
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  override def &(set: MySet[A]): MySet[A] = filter(set)

  override def --(set: MySet[A]): MySet[A] = filter(!set)

  // ![1 2 3] = everything except [1 2 3]
  override def unary_! : MySet[A] = new PropertyBasedSet[A](!contains(_))
}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || elem == x)
  override def ++(set: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || set(x))
  override def map[B](f: A => B): MySet[B] = politelyFail// new PropertyBasedSet[B](x => property(f(x)))
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def filter(p: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && p(x))

  override def foreach(f: A => Unit): Unit = politelyFail
  override def -(elem: A): MySet[A] = filter(x => x != elem)
  override def &(set: MySet[A]): MySet[A] = filter(set)
  override def --(set: MySet[A]): MySet[A] = filter(!set)
  override def unary_! : MySet[A] = new PropertyBasedSet[A](!property(_))
  def politelyFail = throw new IllegalArgumentException("Sorry")
}

object MySet {
  def apply[A](v: A*): MySet[A] = {
    @tailrec
    def buildSet(vSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (vSeq.isEmpty) acc
      else buildSet(vSeq.tail, acc + vSeq.head)
    }
    buildSet(v.toSeq, new EmptySet[A])
  }
}

object MySetPlayGround extends App {
  val s = MySet(1, 2, 3, 4)
  s + -1 ++ MySet(-1, -2) map (_*10) filter (_%3 == 0) foreach println
}