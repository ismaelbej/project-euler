package ismael

object P38 {  
  def adjustArray[T](src: Array[T], i: Int, j: Int) {
    var tmp = src(i)
    src(i) = src(j)
    src(j) = tmp
    var len = (src.length - i)/2
    for (k <- 1 to len) {
      tmp = src(src.length - k)
      src(src.length - k) = src(i + k)
      src(i + k) = tmp
    }
  }
  def prevPermutation[T](src: Array[T])(implicit view:(T) => Ordered[T]):Boolean = {
    var i = src.length - 2
    while (i >= 0 && src(i) <= src(i+1)) i -= 1
    if (i < 0) return false
    var j = src.length - 1
    while (src(j) >= src(i)) j -= 1
    adjustArray(src,i,j) 
    true
  }
  def prevSequence(l: List[Int]): List[Int] = {
    var r: Array[Int] = l.toArray
    prevPermutation(r)
    r toList
  }
  def makeNumber(l: List[Int]): Int = {
    def makeNumberRec(n: Int, l: List[Int]): Int = l match {
      case Nil => n
      case h :: t => makeNumberRec(10*n + h, t)
    }
    makeNumberRec(0, l)
  }
  val pandigitalSequences: Stream[List[Int]] = List(9, 8, 7, 6, 5, 4, 3, 2, 1) #:: pandigitalSequences.map(prevSequence)
  val pandigital = pandigitalSequences map makeNumber
  def getFirstDigits(n: Int, d: Int): Int = {
    val e = countDigits(n)
    val b = power(10, e - d)
    n / b
  }
  def countDigits(n: Int): Int = n match {
    case 0 => 0
    case _ => 1+countDigits(n/10)
  }
  def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
  def getValidSeeds(n: Int): List[Int] = List.range(1, 5) map {getFirstDigits(n, _)}
  def genPalindromic(n: Int): List[Int] = {
    def genPalindromicRec(a: Int, c: Int, d: Int): List[Int] = {
      val b = n*c
      val e = countDigits(b)
      val q = a*power(10, e) + b
      if (d+e == 9) List(q)
      else if (d+e > 9) Nil
      else genPalindromicRec(q, c+1, d+e)
    }
    genPalindromicRec(n, 2, countDigits(n))
  }
  def isPandigitalProduct(n: Int): Boolean = {
    getValidSeeds(n) flatMap genPalindromic exists {_ == n}
  }
  val pandigitalProduct = pandigital filter isPandigitalProduct
  def run(args: Array[String]): Unit = {
    println(pandigitalProduct head)
  }
}
