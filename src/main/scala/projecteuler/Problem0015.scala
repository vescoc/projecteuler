package projecteuler

import scala.annotation.tailrec

import Benchmark._

object Problem0015 {
  type Path = List[(Int, Int)]

  def solveBS(n: Int, m: Int): Set[Path] = {
    @tailrec
    def solve(paths: Set[Path] = Set(List((0, 0))), visited: Set[Path] = Set(), current: Set[Path] = Set()): Set[Path] = {
      if (paths.size == 0)
        current
      else {
        //println(s"solve length: ${paths.head.size} paths: ${paths.size} visited: ${visited.size} current: ${current.size}")

        val candidates = paths
          .filterNot(path => visited.contains(path))
        val (goal, running) = candidates
          .partition(path => {
            val (x, y) = path.head
            x == n - 1 && y == m - 1
          })

        val newPaths = running
          .flatMap(path => {
            val (x, y) = path.head
            for {
              dx <- List(0, 1)
              dy <- List(0, 1)
            } yield
                if (dx != dy && x + dx < n && y + dy < m)
                  Some((x + dx, y + dy) :: path)
                else
                  None
          })
          .flatten

        solve(newPaths, visited ++ paths, current ++ goal)
      }
    }

    solve()
  }

  def solveM(n: Int, m: Int) = {
    def solve(n: Int = n, m: Int = m): Long =
      if (n == 1 || m == 1)
        1
      else
        (for (i <- 1 to n - 1) yield solve(n - i, m - 1)).sum + (for (i <- 1 to m - 1) yield solve(n - 1, m - i)).sum

    solve()
  }

  def solveD(n: Int, m: Int) = {
    val table: collection.mutable.Map[(Int, Int), Long] = collection.mutable.Map()

    def solve(n: Int = n, m: Int = m): Long = {
      if (n == 1 || m == 1) {
        table((n, m)) = 1
        1
      } else {
        table.get((n, m)) match {
          case Some(v) => v
          case None => {
            table((n, m)) = (for (i <- 1 to n - 1) yield solve(n - i, m - 1)).sum + (for (i <- 1 to m - 1) yield solve(n - 1, m - i)).sum
            table((n, m))
          }
        }
      }
    }

    solve()
  }

  def solveD1(n: Int, m: Int) = {
    val table: collection.mutable.Map[(Int, Int), Long] = collection.mutable.Map()

    for (i <- 0 to n)
      table((i, 0)) = 1
    for (i <- 0 to m)
      table((0, i)) = 1

    for (i <- 1 to n)
      for (j <- 1 to m)
        table((i, j)) = table((i - 1, j)) + table((i, j - 1))

    table((n, m))
  }

  def main(args: Array[String]) {
    val N = 10

    bench("BS") {
      for (i <- 2 to N) println(s"test BS $i: " + solveBS(i, i).size)
      for (i <- 2 to N) println(s"test BS $i 1: " + solveBS(i, 1).size)
      for (i <- 2 to N) println(s"test BS 1 $i: " + solveBS(1, i).size)
    }

    bench("M") {
      for (i <- 2 to N) println(s"test M $i: " + solveM(i, i))
      for (i <- 2 to N) println(s"test M $i 1: " + solveM(i, 1))
      for (i <- 2 to N) println(s"test M 1 $i: " + solveM(1, i))
    }

    bench("D") {
      for (i <- 2 to N) println(s"test D $i: " + solveD(i, i))
      for (i <- 2 to N) println(s"test D $i 1: " + solveD(i, 1))
      for (i <- 2 to N) println(s"test D 1 $i: " + solveD(1, i))
    }

    bench("D1") {
      for (i <- 2 to N) println(s"test D1 $i: " + solveD1(i, i))
      for (i <- 2 to N) println(s"test D1 $i 1: " + solveD1(i, 1))
      for (i <- 2 to N) println(s"test D1 1 $i: " + solveD1(1, i))
    }

    bench("problem D") {
      println("problem: " + solveD(21, 21))
    }

    bench("problem D1") {
      println("problem: " + solveD1(20, 20))
    }
  }
}
