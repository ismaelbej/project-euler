package ismael

import ProjectEuler._

object P73 {
  val Max = 12000
  def isCoprime(a: Int, b: Int): Boolean = dcm(a, b) == 1
  def countFractions(n: Int): Long = {
    (1 + n / 3 to (n + 1) / 2 - 1 filter {isCoprime(n, _)}) size
  }
  val fractions = 2 to Max map countFractions
  def run(args: Array[String]): Unit = {
    println(fractions sum)
  }
}
