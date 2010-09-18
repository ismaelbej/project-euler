package ismael

import ProjectEuler._

object P46 {
  val odds = Stream.from(0) map {2*_ + 1}
  val squares = Stream.from(0) map {x => x*x}
  def isGoldbach(n: Int): Boolean = squares.takeWhile(y => 2*y < n).exists(y => isPrime(n - 2*y))
  val nonGoldbach = odds filterNot isGoldbach
  def run(args: Array[String]): Unit = {
    println(nonGoldbach head)
  }
}
