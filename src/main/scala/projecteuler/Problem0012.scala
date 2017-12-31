package projecteuler

import scala.annotation.tailrec

import intmath._

object Problem0012 {
  def solve(limit: Int): Int = {
    @tailrec
    def next(n: Int = 2, current: Int = 1): Int = {
      if (n % 1000 == 0)
        println(s"next $n $current")
      val d = divisors(current)
      if (d.size > limit)
        current
      else
        next(n + 1, current + n)
    }

    next()
  }

  def main(args: Array[String]) {
    println("divisors 1000: " + divisors(1000));
    println("test: " + solve(4))
    println("problem: " + solve(500))
  }
}
