package projecteuler

import scala.annotation.tailrec

object Problem0014 {
  def collatz[T : Integral](n: T) = {
    import scala.language.implicitConversions
    
    val numeric = implicitly[Integral[T]]

    implicit def fromInt(n: Int): T = numeric.fromInt(n)

    @tailrec
    def collatz(current: List[T] = List(), n: T = n): List[T] =
      if (numeric.lt(n, 2))
        1 :: current
      else if (numeric.rem(n, 2) == 0)
        collatz(n :: current, numeric.quot(n, 2))
      else
        collatz(n :: current, numeric.plus(numeric.times(3, n), 1))
    collatz().reverse
  }

  def main(args: Array[String]) {
    println("test: " + collatz(13))

    println("problem: " + (for (n <- 1 until 1 * 1000 * 1000) yield (n, collatz[BigInt](n).size)).maxBy(_._2))
  }
}
