package ismael

import ProjectEuler._

object P58 {
  def square(n: Long): Long = n * n
  def countPrimes(n: Int): Int = {
    val s = square(2*n+1)
    (List(s - 2*n, s - 4*n, s - 6*n) filter isPrime) size
  }
  def sumSquares(p: (Int, Int)): Int = p._1 + p._2
  val primesBySquare = Stream.from(1) map countPrimes
  val totalPrimes: Stream[Int] = 3 #:: (totalPrimes zip primesBySquare.tail map sumSquares)
  val totalPrimesWithIndex = totalPrimes zipWithIndex
  val h = totalPrimesWithIndex dropWhile {x => 10*x._1 >= 5+4*x._2} head
  def run(args: Array[String]): Unit = {
    println(3+2*h._2)
  }
}
