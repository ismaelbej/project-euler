package ismael

import ProjectEuler._

object P37 {
  val validStartDigits = List.range(1, 10)
  val validInternalDigits = List(1, 3, 7, 9)
  def makeNumber(l: List[Int]): Long = {
    def makeNumberRec(n: Long, l: List[Int]): Long = l match {
      case Nil => n
      case h :: t => makeNumberRec(n * 10 + h, t)
    }
    makeNumberRec(0L, l)
  }
  def isRightTruncatable(d: List[Int]): Boolean = {
    def isTruncatableRec(d: List[Int]): Boolean = d match {
      case Nil => true
      case h :: t => if (isPrime(makeNumber(d))) isTruncatableRec(t) else false
    }
    isTruncatableRec(d)
  }
  def genLeftTruncatables(d: List[Int]): List[List[Int]] = {
    if (!isPrime(makeNumber(d))) Nil
    else d :: validInternalDigits.flatMap(x => genLeftTruncatables(d :+ x))
  }
  val leftTruncatables = validStartDigits flatMap {x => genLeftTruncatables(List(x))}
  val truncatablePrimes = leftTruncatables filter isRightTruncatable map makeNumber
  val validTruncatables = truncatablePrimes filter {_ >= 10}
  def run(args: Array[String]): Unit = {
    println(validTruncatables sum)
  }
}
