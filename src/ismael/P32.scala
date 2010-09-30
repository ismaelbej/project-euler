package ismael

object P32 {
  val digits = 1 to 9 toList
  def genDigits(n: Int) = {
    def genDigitsRec(l: List[Int]): List[List[Int]] = {
      if (l.size == n) List(l)
      else digits flatMap {x => genDigitsRec(l :+ x)}
    }
    digits flatMap {x => genDigitsRec(List(x))} map makeNumber
  }
  val twoDigits = genDigits(2)
  val fourDigits = genDigits(4)
  def getDigits(n: Int): Set[Int] = {
    def getDigitsRec(m: Int): List[Int] = m match {
      case 0 => Nil
      case _ => m % 10 :: getDigitsRec(m / 10)
    }
    getDigitsRec(n) toSet
  }
  def makeNumber(l: List[Int]): Int = {
    def makeNumberRec(n: Int, l: List[Int]): Int = l match {
      case Nil => n
      case h :: t => makeNumberRec(n*10 + h, t)
    }
    makeNumberRec(0, l)
  }
  val digitSet = digits toSet
  def isPandigitalProduct(n: Int): Boolean = {
    val digitsN = getDigits(n)
    def isPandProdRec(d: Int): Boolean = {
      if (d*d > n) false
      else if (n % d == 0) {
        val m = n / d
        val di = digitsN ++ getDigits(m) ++ getDigits(d)
        if (digitSet == di.toSet) true
        else isPandProdRec(d + 1)
      }
      else isPandProdRec(d + 1)
    }
    isPandProdRec(2)
  }
  val pandigitalProduct = fourDigits filter isPandigitalProduct
  def run(args: Array[String]): Unit = {
    println(pandigitalProduct sum)
  }
}
