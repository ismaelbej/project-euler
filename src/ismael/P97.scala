package ismael

import ProjectEuler._

object P97 {
  val A = 28433L
  val b = 2L
  val ex = 7830457
  val M = power(10L, 10)
  val Num = (A * powerMod(b, ex, M) + 1) % M
  def run(args: Array[String]): Unit = {
    println(Num)
  }
}
