package projecteuler

import scala.annotation.tailrec

import intmath._

object Problem0006 {
  def solve(numbers: Seq[BigInt]) = {
    @tailrec
    def sumsquares(numbers: Seq[BigInt] = numbers, current: BigInt = 0): BigInt =
      if (numbers.size == 0)
        current
      else
        sumsquares(numbers.tail, current + pow(numbers.head, 2))

    @tailrec
    def squaresums(numbers: Seq[BigInt] = numbers, current: BigInt = 0): BigInt =
      if (numbers.size == 0)
        pow(current, 2)
      else
        squaresums(numbers.tail, current + numbers.head)

    squaresums() - sumsquares()
  }

  def main(args: Array[String]) {
    import scala.language.implicitConversions

    implicit def toBigIntSeq(seq: Seq[Int]): Seq[BigInt] = seq.map(v => BigInt(v))

    println("test: " + solve(1 to 10))
    println("problem: " + solve(1 to 100))
  }
}
