package ismael

import ProjectEuler._

object P69 {
  val Max = 1000000
  def calcMinimum(n: Int): Int = {
    def calcMinimumRec(p: Int, q: Stream[Int]): Int = {
      val h = q.head
      if (p * h > n) p
      else calcMinimumRec(p * h, q tail)
    }
    calcMinimumRec(1, primes)
  }
  def run(args: Array[String]): Unit = {
    println(calcMinimum(Max))
  }
}
