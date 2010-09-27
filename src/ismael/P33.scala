package ismael

import ProjectEuler._

object P33 {
  def isEquivFrac(a: Int, b: Int, d: Int): Boolean = {
    val ab = 10 * a + b
    val bd = 10 * b + d
    ab * d == bd * a
  }
  def isNonTrivialFrac(a: Int, d:Int): Boolean = {
    1 to 9 exists {x => isEquivFrac(a, x, d)}
  }
  def run(args: Array[String]): Unit = {
    val frac = for (a <- 1 to 8; d <- a + 1 to 9 if isNonTrivialFrac(a, d)) yield (a, d)
    val (num, denom) = ((1, 1) /: frac) {(x, y) => (x._1 * y._1, x._2 * y._2)}
    val d = dcm(num, denom)
    println(denom / d)
  }
}
