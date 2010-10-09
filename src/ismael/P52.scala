package ismael

import ProjectEuler._

object P52 {
  def getDigits(n: Long): List[Int] = {
    def getDigitsRec(n: Long): List[Int] = n match {
      case 0 => Nil
      case _ => ((n % 10) toInt) :: getDigitsRec(n / 10)
    }
    getDigitsRec(n) sortWith {_ < _}
  }
  def nextInterval(p: (Int, Int)): (Int, Int) = (p._1 * 10, p._2 * 10 + 6)
  val intervals: Stream[(Int, Int)] = (1, 1) #:: (intervals map nextInterval)
  def hasSameDigits(n: Long): Boolean = {
    val digits = getDigits(n)
    2 to 6 forall {x => getDigits(x * n) == digits}
  }
  def findSameDigitsProducts(a: Long, b: Long): List[Long] = {
    a to b find hasSameDigits toList
  }
  val sameDigitsProducts = intervals flatMap {x => findSameDigitsProducts(x._1, x._2)}
  def run(args: Array[String]): Unit = {
    println(sameDigitsProducts head)
  }
}
