package ismael

import ProjectEuler._

object P35 {
  val Max = 1000000
  def isCircular(p: Int): Boolean = {
    val l = countDigits(p)
    def rotateLeft(d: Int): Int = {
      val b = power(10, l - d)
      val c = power(10, d)
      val a = p/b
      (p - a * b) * c + a
    }
    (1 to l - 1) forall {x => isPrime(rotateLeft(x))}
  }
  val circularPrimes = primes takeWhile {_ < Max} filter isCircular
  def run(args: Array[String]): Unit = {
    println(circularPrimes length)
  }
}
