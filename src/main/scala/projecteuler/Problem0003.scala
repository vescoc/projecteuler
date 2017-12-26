package projecteuler

import scala.annotation.tailrec

object Problem0003 {
  @tailrec
  def solve(current: Long, divisor: Long): Long = {
    if (current <= divisor)
      current
    else
      if (current % divisor == 0) {
        solve(current / divisor, divisor)
      } else {
        solve(current, divisor + 1)
      }
  }

  def main(args: Array[String]) {
    println("test: " + solve(13195, 2))
    println("test: " + solve(600851475143L, 2))
  }
}
