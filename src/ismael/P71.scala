package ismael

import ProjectEuler._

object P71 {
  val Max = 1000000
  // Farey Sequence
  // http://mathworld.wolfram.com/FareySequence.html
  // Si p/q < p'/q' son fracciones contiguas entonces cumplen que
  //   p' q - p q' = 1
  def calcMaxDenom(m: Int): Int = ((m - 5) / 7) * 7 + 5
  def calcMaxNum(m: Int): Int = (3 * calcMaxDenom(m) - 1) / 7
  def run(args: Array[String]): Unit = {
    println(calcMaxNum(Max))
  }
}
