package ismael

import ProjectEuler._

object P40 {
  def countNumbers(n: Int): Int = n match {
    case 0 => 0
    case _ => n * (power(10, n) - power(10, n - 1))
  }
  def findDigitPosition(n: Int): (Int, Int) = {
    def findPositionRec(n: Int, d: Int): (Int, Int) = {
      val h = countNumbers(d)
      if (n <= h) (d, n)
      else findPositionRec(n - h, d + 1)
    }
    findPositionRec(n, 1)
  }
  def findDigit(n: Int): Int = {
    val (d, p) = findDigitPosition(n)
    val b = (p - 1) / d
    val r = d - 1 - (p - 1) % d
    val m = power(10, d - 1) + b
    (m / power(10, r)) % 10
  }
  def run(args: Array[String]): Unit = {
    val digits = List(1, 10, 100, 1000, 10000, 100000, 1000000) map findDigit
    println((1 /: digits) {_*_})
  }
}
