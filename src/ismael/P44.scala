package ismael

import ProjectEuler._

object P44 {
  def makePentagonal(n: Long): Long = n * (3 * n - 1) / 2
  def calcPentagonal(p: Long): Long = {
    val r = 1 + 24 * p
    val s = math.sqrt(r).toLong
    val t = 1 + s
    t / 6
  }
  def isPentagonal(p: Long): Boolean = {
    val r = calcPentagonal(p)
    def isPentRec(r: Long): Boolean = {
      val j = makePentagonal(r)
      if (j > p) false
      else if (j == p) true
      else isPentRec(r + 1)
    }
    isPentRec(r)
  }
  def findPentDiff(a: Long, b: Long, m: Long): Boolean = {
    val bb = makePentagonal(m + 1)
    val c = a + b
    if (c < bb) false
    else if (isPentagonal(c)) {
      if (isPentagonal(b + c)) true
      else findPentDiff(a, bb, m + 1)
    }
    else findPentDiff(a, bb, m + 1)
  }
  def findDifference(n: Long): Long = {
    def findDifferenceRec(a: Long, n: Long): Long = {
      val b = makePentagonal(n + 1)
      if (findPentDiff(a, 1, 1)) a
      else findDifferenceRec(b, n + 1)
    }
    findDifferenceRec(makePentagonal(n), n)
  }
  def run(args: Array[String]): Unit = {
    val r = findDifference(1)
    println(r)
  }
}
