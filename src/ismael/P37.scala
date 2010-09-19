package ismael

import ProjectEuler._

object P37 {
  val validDigits = List.range(1, 10)
  def isPrime(n: BigInt): Boolean = {
    def sqr(n: Int): BigInt = BigInt(n) * n
    if (n==1) false
    else primes takeWhile {x => sqr(x) <= n} forall {n%_!=0}
  }
  def makeNumber(l: List[Int]): BigInt = {
    def makeNumberRec(n: BigInt, l: List[Int]): BigInt = l match {
      case Nil => n
      case h :: t => makeNumberRec(n*10 + h, t)
    }
    makeNumberRec(0, l)
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
    else d :: validDigits.flatMap(x => genLeftTruncatables(d :+ x))
  }
  val leftTruncatables = validDigits flatMap {x => genLeftTruncatables(List(x))}
  val truncatablePrimes = leftTruncatables filter isRightTruncatable map makeNumber
  val validTruncatables = truncatablePrimes filter {_>=10}
  def run(args: Array[String]): Unit = {
    println(validTruncatables sum)
  }
}
