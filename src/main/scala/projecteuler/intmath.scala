package projecteuler

import scala.annotation.tailrec

trait IntMathTrait {
  lazy val prime: Stream[Int] = Prime[Int]

  object Prime {
    private var map: Map[Any, Any] = Map()

    def apply[T : Integral](implicit m: Manifest[T]): Stream[T] = {
      if (!map.contains(m))
        map = map + (m -> prime[T])
      map(m).asInstanceOf[Stream[T]]
    }

    def prime[T : Integral]: Stream[T] = {
      val numeric = implicitly[Integral[T]]

      val zero = numeric.zero
      val one = numeric.one
      val two = numeric.plus(numeric.one, numeric.one)

      lazy val p: Stream[T] = one #:: two #:: {
        def next(n: Int, current: T): Stream[T] = {
          @tailrec
          def find(current: T = numeric.plus(current, one)): T = {
            val half = numeric.quot(current, two)

            @tailrec
            def check(s: Stream[T] = p.tail): Boolean = {
              val head = s.head
              if (numeric.gt(head, half))
                true
              else if (numeric.equiv(numeric.rem(current, head), zero))
                false
              else
                check(s.tail)
            }

            if (check())
              current
            else
              find(numeric.plus(current, one))
          }

          val v = find()

          v #:: next(n + 1, v)
        }

        next(2, two)
      }

      p
    }
  }

  def factor[T : Integral](n: T)(implicit m: Manifest[T]) = {
    val numeric = implicitly[Integral[T]]

    val prime = Prime.apply

    val zero = numeric.zero
    val one = numeric.one

    @tailrec
    def factor(idx: Int = 1, n: T = n, f: Int = 0, current: Seq[(T, Int)] = List((one -> 1))): Seq[(T, Int)] = {
      val p = prime(idx)
      if (numeric.lteq(n, one)) {
        if (f == 0)
          current
        else
          (p -> f) +: current
      } else if (numeric.equiv(numeric.rem(n, p), zero))
        factor(idx, numeric.quot(n, p), f + 1, current)
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
  
  def mcm[T : Integral](numbers: Seq[T])(implicit m: Manifest[T]) = {
    val numeric = implicitly[Integral[T]]

    @tailrec
    def mcm(numbers: Seq[T] = numbers, current: Map[T, Int] = Map()): Map[T, Int] =
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

    mcm().foldLeft(numeric.one)((r, c) => numeric.times(r, pow(c._1, c._2)))
  }

  def divisors[T : Integral](n: T)(implicit m: Manifest[T]) = {
    val numeric = implicitly[Integral[T]]

    val zero = numeric.zero
    val one = numeric.one
    val two = numeric.plus(one, one)

    val half = numeric.quot(n, two)

    @tailrec
    def next(d: T = one, current: Set[T] = Set()): Set[T] = {
      if (numeric.gt(d, half))
        current + n
      else if (numeric.equiv(numeric.rem(n, d), zero))
        next(numeric.plus(d, one), current + d)
      else
        next(numeric.plus(d, one), current)
    }

    next()
  }
}

object intmath extends IntMathTrait
