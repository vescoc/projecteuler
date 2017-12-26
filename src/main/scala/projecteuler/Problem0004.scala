package projecteuler

import scala.annotation.tailrec

object Problem0004 {
  def solve(n: Int) = {
    def pow(n: Int, e: Int): Int = {
      @tailrec
      def p(current: Int = 1, e: Int = e): Int =
        if (e == 0)
          current
        else
          p(current * n, e - 1)

      p()
    }

    val m = pow(10, n)
    val l = pow(10, n - 1)

    @tailrec
    def test(current: Int = m - 1): Option[(Int, Int, Int)] = {
      val target = (current.toString + current.toString.reverse).toInt

      @tailrec
      def check(divisor: Int = l): Option[(Int, Int, Int)] =
        if (divisor == m)
          None
        else if (target % divisor == 0) {
          val r = target / divisor
          if (r >= l && r < m)
            Some((target, divisor, target / divisor))
          else
            check(divisor + 1)
        } else
            check(divisor + 1)

      check() match {
        case r @ Some(_) => r
        case None => test(current - 1)
      }
    }

    test()
  }

  def main(args: Array[String]) {
    println("test: " + solve(2))
    println("problem: " + solve(3))
  }
}
