package problems

import scala.annotation.tailrec

object Task extends App {

  /**
    * defines how many times the sign of numbers in a sequence changes
    */
  def times(seq: Seq[Int]): Int = {
    @tailrec
    def go(n: Int, prev: Int, seq: Seq[Int]): Int = {
      seq match {
        case Nil => n
        case h :: t =>
          if ((prev > 0 && h < prev) || (prev < 0 && h > prev)) go(n + 1, h, t) else go(n, h, t)
      }
    }
    go(0, 0, seq)
  }

  val s = Seq(-1, -4, -5, 5, -3, 2, 2, 0, -10)
  print(times(s))
}
