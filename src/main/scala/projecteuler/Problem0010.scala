package projecteuler

import scala.annotation.tailrec

object Problem0010 {
  def solve(limit: Int) = {
    @tailrec
    def sum(step: Int = 0, current: Long = 0, prime: Stream[Int] = intmath.prime.tail): Long = {
      val p = prime.head
      if (step % 1000 == 0)
        println(s"current step=$step prime=$p")
      if (p < limit)
        sum(step + 1, current + p, prime.tail)
      else
        current
    }
      
    sum()
  }

  def main(args: Array[String]) {
    println("test: " + solve(10))
    println("problem: " + solve(2 * 1000 * 1000))
  }
}
