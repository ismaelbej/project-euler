package ismael

object P36 {
  val digits = List.range(0, 10)
  val startDigits = List(1, 3, 5, 7, 9)
  def makeNumber(l: List[Int]): Int = {
    def makeNumberRec(b: Int, l: List[Int]): Int = l match {
      case Nil => b
      case h :: t => makeNumberRec(b * 10 + h, t)
    }
    makeNumberRec(0, l)
  }
  def genPalindromic(d: Int): List[Int] = {
    val n = (d + 1) / 2
    def genPalindromicDigits(l: List[Int]): List[List[Int]] = {
      if (l.length >= n) List(l)
      else digits flatMap {x => genPalindromicDigits(l :+ x)}
    }
    def makePalindromic(l: List[Int]): Int = {
      val r = l.reverse
      if ((d & 1) == 0) makeNumber(l ++ r)
      else makeNumber(l ++ r.tail)
    }
    val palindromicDigits = startDigits flatMap {x => genPalindromicDigits(List(x))}
    palindromicDigits map makePalindromic
  }
  def reverseInt(n: Int, b: Int): Int = {
    def reverseRec(r: Int, n: Int): Int = n match {
      case 0 => r
      case _ => reverseRec(r * b + n % b, n / b)
    }
    reverseRec(0, n)
  }
  def isPalindromicBase(n: Int, b: Int): Boolean = n == reverseInt(n, b)
  val palindromics = 1 to 6 flatMap genPalindromic filter {x => isPalindromicBase(x, 2)}
  def run(args: Array[String]): Unit = {
    println(palindromics sum)
  }
}
