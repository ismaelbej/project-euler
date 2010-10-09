package ismael

import ProjectEuler._

object P55 {
  val Max = 10000
  def isLychrel(n: Int): Boolean = {
    def reverseNumber(n: BigInt): BigInt = {
      def reverseRec(a: BigInt, b: BigInt): BigInt ={
        if (a == 0) b
        else reverseRec(a / 10, b * 10 + a % 10)
      }
      reverseRec(n, 0)
    }
    val MaxIter = 50
    def isLychrelBelow(n: BigInt, c: Int): Boolean = {
      val m = reverseNumber(n)
      if (m == n) false
      else if (c == 0) true
      else isLychrelBelow(m + n, c - 1)
    }
    isLychrelBelow(reverseNumber(n) + n, 50)
  }
  val lychrelNumbers = 1 to Max filter isLychrel
  def run(args: Array[String]): Unit = {
    println(lychrelNumbers size)
  }
}
