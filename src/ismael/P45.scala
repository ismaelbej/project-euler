package ismael

import ProjectEuler._

object P45 {
  def makeHexagonal(n: Long): Long = n * (2 * n - 1)
  val hexagonalNumbers = Stream.from(1) map {x => makeHexagonal(x)}
  def isPentagonal(h: Long): Boolean = {
    val r = 1 + 24 * h
    val s = math.sqrt(r).toLong
    if (s * s != r) false
    else {
      val t = 1 + s
      if (t % 6 != 0) false
      else true
    }
  }
  val pentaHexaTri = hexagonalNumbers filter isPentagonal drop 2
  def run(args: Array[String]): Unit = {
    println(pentaHexaTri head)
  }
}
