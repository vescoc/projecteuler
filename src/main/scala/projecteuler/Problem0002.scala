package projecteuler

import scala.annotation.tailrec

object Problem0002 {
  @tailrec
  def solve(f_1: Int = 1, f_2: Int = 2, current: Int = 2, limit: Int = 4 * 1000 * 1000): Int = {
    val f = f_1 + f_2
    if (f >= limit)
      current
    else
      solve(f_2, f, if (f % 2 == 0) current + f else current, limit)
  }

  def main(args: Array[String]) {
    println("problem: " + solve())
  }
}
