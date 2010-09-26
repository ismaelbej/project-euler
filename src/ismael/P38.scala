package ismael

import ProjectEuler._

object P38 {  
  def prevSequence(l: List[Int]): List[Int] = {
    var r: Array[Int] = l.toArray
    prevPermutation(r)
    r toList
  }
  def makeNumber(l: List[Int]): Int = {
    def makeNumberRec(n: Int, l: List[Int]): Int = l match {
      case Nil => n
      case h :: t => makeNumberRec(10 * n + h, t)
    }
    makeNumberRec(0, l)
  }
  def splitDigits(n: Int, d: Int): (Int, Int) = {
    val r = power(10, d)
    (n / r, n % r)
  }
  def isPandigitalWith(n: Int, d: Int): Boolean = {
    val (m, r) = splitDigits(n, 9 - d)
    def isPandigitalWithRec(c: Int, r: Int, d: Int): Boolean = {
      if (r == 0) true
      else {
        val o = m * c
        val e = countDigits(o)
        val (oo, rr) = splitDigits(r, d - e)
        if (oo != o) false
        else isPandigitalWithRec(c + 1, rr, d - e)
      }
    }
    isPandigitalWithRec(2, r, 9 - d)
  }
  def isPandigitalProduct(n: Int): Boolean = {
    1 to 4 exists {isPandigitalWith(n, _)}
  }
  val pandigitalSequences: Stream[List[Int]] = List(9, 8, 7, 6, 5, 4, 3, 2, 1) #:: pandigitalSequences.map(prevSequence)
  val pandigital = pandigitalSequences map makeNumber
  val pandigitalProducts = pandigital filter isPandigitalProduct
  def run(args: Array[String]): Unit = {
    println(pandigitalProducts head)
  }
}
