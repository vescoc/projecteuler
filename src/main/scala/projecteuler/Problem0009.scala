package projecteuler

object Problem0009 {
  def main(args: Array[String]) {
    val pythagoreanTriplet = for {
      a <- 0L to (1000L - 2L)
      b <- (a + 1L) to (1000L - 1L)
      c <- (b + 1L) to 1000L if (a + b + c == 1000L && a * a + b * b == c * c)
        } yield (a, b, c)

    println("problem: " + pythagoreanTriplet.map(v => (v -> v._1 * v._2 * v._3)))
  }
}
