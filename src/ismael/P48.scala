package ismael

import ProjectEuler._

object P48 {
  val M = power(10L, 10)
  def sumMod(a: Long, b: Long, M: Long): Long = ((BigInt(a) + b) % M) toLong
  val powerSequence = 1 to 1000 map {x => powerMod(x, x, M)}
  val sumPowerSequence = powerSequence.foldLeft(0L) {(x, y) => sumMod(x, y, M)}
  def run(args: Array[String]): Unit = {
    println(sumPowerSequence)
  }
}
