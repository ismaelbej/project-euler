package ismael

import ProjectEuler._

object P97 {
  val A = 28433L
  val b = 2L
  val ex = 7830457
  val M = power(10L, 10)
  val Num = (A * powerMod(b, ex, M) + 1) % M
  def powerMod(b: Long, ex: Long, M: Long): Long = {
    def powerModRec(p: Long, b: Long, ex: Long): Long = ex match {
      case 0 => p
      case _ => {
        val bb = (BigInt(b) * b) % M        
        if ((ex&1)==0) {
          powerModRec(p, bb toLong, ex/2)
        }
        else {
          val pb = (BigInt(p) * b) % M
          powerModRec(pb toLong, bb toLong, ex/2)
        }
      }
    }
    powerModRec(1, b%M, ex) 
  }
  def run(args: Array[String]): Unit = {
    println(Num)
  }
}
