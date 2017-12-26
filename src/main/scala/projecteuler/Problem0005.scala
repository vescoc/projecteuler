package projecteuler

import scala.annotation.tailrec

object Problem0005 {
  lazy val prime: Stream[Int] = 1 #:: 2 #:: {
    def next(n: Int, current: Int): Stream[Int] = {
      val l = prime.tail.take(n - 1).toList

      @tailrec
      def find(current: Int = current + 1): Int =
        if (l.forall(d => current % d != 0))
          current
        else
          find(current + 1)

      val p = find()

      p #:: next(n + 1, p)
    }

    next(2, 2)
  }

  def factor(n: Int) = {
    @tailrec
    def factor(idx: Int = 1, n: Int = n, f: Int = 0, current: Seq[(Int, Int)] = List((1 -> 1))): Seq[(Int, Int)] = {
      val p = prime(idx)
      if (n <= 1) {
        if (f == 0)
          current
        else
          (p -> f) +: current
      } else if (n % p == 0)
        factor(idx, n / p, f + 1, current)
      else if (f == 0)
        factor(idx + 1, n, 0, current)
      else
        factor(idx + 1, n, 0, (p -> f) +: current)
    }

    factor()
  }

  def pow(n: Int, e: Int) = {
    @tailrec
    def p(current: Int = 1, e: Int = e): Int =
      if (e <= 0)
        current
      else
        p(current * n, e - 1)

    p()
  }

  def mcm(numbers: Seq[Int]) = {
    @tailrec
    def mcm(numbers: Seq[Int] = numbers, current: Map[Int, Int] = Map()): Map[Int, Int] =
      if (numbers.size == 0)
        current
      else {
        val head = numbers.head
        val merge = {
          val map = factor(head).toMap
          (map.keySet ++ current.keySet).map(k => (k -> math.max(map.getOrElse(k, 0), current.getOrElse(k, 0)))).toMap
        }

        mcm(numbers.tail, merge)
      }

    mcm().foldLeft(1)((r, c) => r * pow(c._1, c._2))
  }

  def solve(r: Seq[Int]) = mcm(r)

  def main(args: Array[String]) {
    println("test: " + solve(1 to 10))
    println("problem: " + solve(1 to 20))
  }
}
