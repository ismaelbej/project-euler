package ismael

import math._

object P85 {
  val C = 2000000L
  def calcRects(m: Long, n: Long): Long = {
    m * (m + 1) / 2 * n * (n + 1) / 2
  }
  def calcNearestRoot(n: Long): (Long, Long) = {
    // n(n+1)/2 m(m+1)/2 = C
    // m^2 + m - C / A = 0
    val A = n * (n + 1.0) / 4.0
    val D = sqrt(1.0 + 4.0 * C / A)
    val m = ((-1.0 + D) / 2.0) toLong
    val q = calcRects(m, n)
    val qq = q * (m + 2) / m
    if (C - q < qq - C) (m, C - q)
    else (m + 1, qq - C)
  }
  def calcClosestArea: Long = {
    def calcClosestAreaRec(r: Long, q: Long, d: Long, n: Long): Long = {
      val (mm, dd) = calcNearestRoot(n)
      if (n > mm) q
      else if (dd < d) calcClosestAreaRec(n, n * mm, dd, n + 1)
      else calcClosestAreaRec(r, q, d, n + 1)
    }
    calcClosestAreaRec(0, 0, C, 1)
  }
  def run(args: Array[String]): Unit = {
    println(calcClosestArea)
  }
}
