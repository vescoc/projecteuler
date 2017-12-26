package projecteuler

import scala.annotation.tailrec

import intmath._

object Problem0007 {
  def solve(n: Int) = prime.drop(n).head

  def main(args: Array[String]) {
    println("test: " + solve(6))
    println("problem: " + solve(10001))
  }
}
