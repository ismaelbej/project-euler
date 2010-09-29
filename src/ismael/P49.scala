package ismael

import ProjectEuler._

object P49 {
  val fourDigitsPrimes = primes dropWhile {_ < 1000} takeWhile {_ < 10000} toList
  def getDigits(n: Int): List[Int] = {
    def getDigitsRec(n: Int): List[Int] = n match {
      case 0 => Nil
      case _ => n % 10 :: getDigitsRec(n / 10)
    }
    getDigitsRec(n) sortWith {_ < _}
  }
  def isPermutation(n: Int)(m: Int): Boolean = getDigits(n) == getDigits(m)
  def getArithmeticSequence(n: Int): List[(Int, Int, Int)] = {
    def isPrimePermutation(x: Int): Boolean = isPrime(x) && isPermutation(n)(x)
    val r = fourDigitsPrimes dropWhile {_ <= n} filter {x => isPermutation(n)(x)}
    r flatMap {x => if (isPrimePermutation((n + x) / 2)) List((n, (n + x) / 2, x)) else Nil} toList
  }
  def run(args: Array[String]): Unit = {
    val primePermuationSequences = fourDigitsPrimes flatMap getArithmeticSequence
    println(primePermuationSequences toList)
  }
}
