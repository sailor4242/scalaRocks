package problems

import scala.annotation.tailrec

object HackerRank extends App {
  val a = new Object()
  val s = "abcdefghhgfedecba"
  var map: Map[Char, Array[Char]] = Map()
  val slice = s.toCharArray.slice(0, 2)
  val c = slice.sum
  map += (slice.sum -> slice)

  def f(n: Int): Unit = if (n > 0) {
    println("Hello World")
    f(n-1)
  }

  val m = s.groupBy(_.toChar).mapValues(_.length)
  m.foldLeft(0)((c, p) => {if (p._2 > 2) c-1 else if (p._2 == 2) c+1 else c}) match {
    case r if r == 1 => println("YES")
    case _ => println("NO")
  }

  //(0 until 4).map(i => 5).flatten
  def f(num:Int,arr:List[Int]):List[Int] = arr.flatMap(List.fill(num)(_))
  def f(arr:List[Int]):List[Int] = arr.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
  def rev(arr:List[Int]):List[Int] = arr match {
    case head::tail => rev(tail) ++ List(head)
    case _ => Nil
  }
  def sum(arr:List[Int]):Int =arr.filter(_ % 2 == 0).sum
  def size(arr:List[Int]):Int = arr.foldLeft(0)((a, _) => a + 1)
  def abs(arr:List[Int]):List[Int] = arr match {   // other solution could be arr.map(Math.abs)
    case h::tail =>
      if (h < 0)
        List(Math.abs(h)) ++ abs(tail)
      else
        List(h) ++ abs(tail)
    case _ => Nil
  }

  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    ar.groupBy(_.toInt).mapValues(_.length).foldLeft(0)((acc, v) => if (v._2/2 > 0) acc + v._2/2 else acc)
  }

  def fibonacci1(x: Int): Int = {
    @tailrec
    def fib (x: Int, y: Int, acc: Int = 0): Int = x match {
      case 0 => acc
      case _ => fib(x - 1, acc, y + acc)
    }
    fib(x, 1)
  }

  def fibonacci2(x: Int): Int = {
    if (x <= 1) x
    else
      (1 until x).foldLeft((0, 1)) {
        case ((a, b), _) => (b, a + b)
      }._1
  }

  object Solution {

    def merge(p: String, q: String): String = {
      if (p.isEmpty) ""
      else StringBuilder.newBuilder
        .append(p.head)
        .append(q.head)
        .append(merge(p.tail, q.tail)).toString()
    }

    def merge2(p: String, q: String): String = {
      p.toCharArray.zip(q.toCharArray).toString
    }

    def main(args: Array[String]) {
      val s = io.Source.stdin.getLines().take(2).toList
      println(merge(s(0), s(1)))
    }
  }

}
