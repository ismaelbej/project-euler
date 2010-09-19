package ismael

import ProjectEuler._

object P10 {
  val N = 2000000
  val primesBelowN = primes takeWhile {_<=N}
  def run(args: Array[String]): Unit = {
    println(primesBelowN.foldLeft(0L) {_+_})
  }
}
