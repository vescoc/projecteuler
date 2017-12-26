package projecteuler

import scala.annotation.tailrec

trait IntMathTrait {
  lazy val prime: Stream[Int] = 1 #:: 2 #:: {
    def next(n: Int, current: Int): Stream[Int] = {
      @tailrec
      def find(current: Int = current + 1): Int = {
        val half = current / 2

        @tailrec
        def check(s: Stream[Int] = prime.tail): Boolean = {
          val head = s.head
          if (head > half)
            true
          else if (current % head == 0)
            false
          else
            check(s.tail)
        }

        if (check())
          current
        else
          find(current + 1)
      }

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

  def pow[T : Numeric](n: T, e: Int) = {
    val numerics = implicitly[Numeric[T]]

    @tailrec
    def p(current: T = numerics.one, e: Int = e): T =
      if (e <= 0)
        current
      else
        p(numerics.times(current, n), e - 1)

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
}

object intmath extends IntMathTrait
