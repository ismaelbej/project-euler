package ismael

import ProjectEuler._

object P72 {
  def calcPhi(n: Int): Long = {
    def reduce(m: Int, d: Int): Int = {
      def reduceRec(m: Int): Int = {
        if (m % d != 0) m
        else reduceRec(m / d)
      }
      reduceRec(m)
    }
    def calcPhiRec(p: Int, m: Int, q: Stream[Int]): Int = {
      val d = q.head
      if (m == 1) p
      else if (d*d > m) p / m * (m - 1)
      else if (m % d != 0) calcPhiRec(p, m, q.tail)
      else calcPhiRec(p / d * (d - 1), reduce(m, d), q.tail)
    }
    calcPhiRec(n, n, primes)
  }
  val Max = 1000000
  val reducecFractions = 2 to Max map calcPhi
  def run(args: Array[String]): Unit = {
    println(reducecFractions sum)
  }
}
