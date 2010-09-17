package ismael

object P41 {
  def prevSequence(l: List[Int]): List[Int] = {
    var r: Array[Int] = l.toArray
    ProjectEuler.prevPermutation(r)
    r toList
  }
  def makeNumber(l: List[Int]): Int = {
    def makeNumberRec(n: Int, l: List[Int]): Int = l match {
      case Nil => n
      case h :: t => makeNumberRec(10*n + h, t)
    }
    makeNumberRec(0, l)
  }
  val pandigitalSequences: Stream[List[Int]] = List(7, 6, 5, 4, 3, 2, 1) #:: pandigitalSequences.map(prevSequence)
  val pandigital = pandigitalSequences map makeNumber
  val pandigitalPrimes = pandigital filter ProjectEuler.isPrime
  def run(args: Array[String]): Unit = {
    println(pandigitalPrimes head)
  }
}
