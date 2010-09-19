package ismael

import ProjectEuler._

object P12 {
  def reduceDiv(n: Int, d: Int): (Int, Int) = {
    def reduceDivRec(n: Int, c: Int): (Int, Int) = {
      if (n%d != 0) (c, n)
      else reduceDivRec(n/d, c+1)
    }
    reduceDivRec(n, 0)
  }
  def countDiv(n: Int): Int = {
    def countDivRec(n: Int, p: Int, d: Int): Int = {
      if (n == 1) p
      else if (d*d > n) 2*p
      else {
	val (c, m) = reduceDiv(n, d)
	countDivRec(m, p * (c + 1), d + 1)
      }
    }
    countDivRec(n, 1, 2)
  }
  def countTriangleNumberDiv(n: Int): Int = {
    if ((n&1) == 0) {
      countDiv(n / 2) * countDiv(n + 1)
    }
    else {
      countDiv(n) * countDiv((n + 1) / 2)
    }
  }
  def makeTriangleNumber(n: Int): Int = n * (n + 1) / 2
  val triangleNumbersFiveHundDiv = Stream.from(1) filter {countTriangleNumberDiv(_) > 500}
  def run(args: Array[String]): Unit = {
    println(makeTriangleNumber(triangleNumbersFiveHundDiv head))
  }
}
