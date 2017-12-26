package projecteuler

import scala.annotation.tailrec

object Problem0005 {
  import intmath._
  
  def solve(r: Seq[Int]) = mcm(r)

  def main(args: Array[String]) {
    println("test: " + solve(1 to 10))
    println("problem: " + solve(1 to 20))
  }
}
