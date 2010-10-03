package ismael

import ProjectEuler._

object P72 {
  def calcPhi(n: Int): Long = {
    def countDiv(m: Int, d: Int): (Int, Int) = {
      def countDivRec(p: Int, m: Int): (Int, Int) = {
        if (m % d != 0) (p, m)
        else countDivRec(p * d, m / d)
      }
      if (m % d == 0) countDivRec(d - 1, m / d)
      else (1, m)
    }
    def calcPhiRec(p: Int, m: Int, q: Stream[Int]): Int = {
      val d = q.head
      if (m == 1) p
      else if (d*d > m) p * (m - 1)
      else {
        val (r, mm) = countDiv(m, d)
        calcPhiRec(p * r, mm, q.tail)
      }
    }
    calcPhiRec(1, n, primes)
  }
  val Max = 1000000
  val reducecFractions = 2 to Max map calcPhi
  def run(args: Array[String]): Unit = {
    println(reducecFractions sum)
  }
}
