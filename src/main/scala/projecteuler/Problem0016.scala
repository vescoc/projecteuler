package projecteuler

import intmath._

object Problem0016 {
  def solve(n: Int): Int =
    pow(BigInt(2), n)
      .toString
      .map(c => c - '0')
      .sum

  def main(args: Array[String]) {
    println("test: " + solve(15))

    println("problem: " + solve(1000))
  }
}
