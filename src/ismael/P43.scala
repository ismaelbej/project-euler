package ismael

object P43 {
  val primes = List(17, 13, 11, 7, 5, 3, 2)
  val digits = List.range(0, 10)
  def makeNumber(l: List[Int]): Long = {
    def makeNumberRec(n: Long, l: List[Int]): Long = l match {
      case Nil => n
      case h :: t => makeNumberRec(n*10 + h, t)
    }
    makeNumberRec(0, l reverse)
  }
  val pandigitalBase = for (a <- 0 to 9; b <- 0 to 9 if (a!=b); c <- 0 to 9 if (a!=c && b!=c)) yield List(a, b, c)
  def generatePandigital(d: List[Int], n: List[Int], r: List[Int], p: List[Int]): List[List[Int]] = p match {
    case h :: t => {
      if (makeNumber(n) % h != 0) Nil
      else r.flatMap(x => generatePandigital(d :+ x, n.tail :+ x, r filterNot (_==x), t))
    }
    case Nil => List(d)
  }
  val pandigitalsWithDivisibility = pandigitalBase.flatMap(x => generatePandigital(x, x, digits filterNot (x contains), primes)) map makeNumber
  def run(args: Array[String]): Unit = {
    println(pandigitalsWithDivisibility sum)
  }
}
