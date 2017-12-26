package projecteuler

import scala.annotation.tailrec

object Problem0001 {
  @tailrec
  def solve(n: Int = 1, current: Int = 0, limit: Int = 10): Int =
    if (n == limit)
      current
    else
      solve(n + 1, current + (if (n % 3 == 0 || n % 5 == 0) n else 0), limit)

  def main(args: Array[String]) {
    println("test: " + solve())
    println("problem: " + solve(limit = 1000))
  }
}
