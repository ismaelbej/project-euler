package ismael

import ProjectEuler._

object P48 {
  val M = power(10L, 10)
  def sumMod(a: Long, b: Long, M: Long): Long = {
    val s = BigInt(a)
    ((s + b) % M) toLong
  }
  val powerSequence = List.range(1, 1001) map {x => powerMod(x, x, M)}
  val sumPowerSequence = powerSequence.foldLeft(0L)((x, y) => sumMod(x, y, M))
  def run(args: Array[String]): Unit = {
    println(sumPowerSequence)
  }
}
