package ismael

object ProjectEuler {
  // power
  def power[T](base: T, ex: Int)(implicit numeric: Numeric[T]): T = {
    def powerRec(prod: T, base: T, ex: Int): T = ex match {
      case 0 => prod
      case _ => {
        if ((ex&1) != 0) powerRec(numeric.times(base, prod), numeric.times(base, base), ex/2)
        else powerRec(prod, numeric.times(base, base), ex/2)
      }
    }
    powerRec(numeric.one, base, ex)
  }
  // esta definicion es redundante en vista de la definicion de arriba
  // pero asi no tengo que crear una clase ModInteger
  def powerMod(b: Long, ex: Long, M: Long): Long = {
    def powerModRec(p: Long, b: Long, ex: Long): Long = ex match {
      case 0 => p
      case _ => {
        val bb = (BigInt(b) * b) % M        
        if ((ex&1)==0) {
          powerModRec(p, bb toLong, ex/2)
        }
        else {
          val pb = (BigInt(p) * b) % M
          powerModRec(pb toLong, bb toLong, ex/2)
        }
      }
    }
    powerModRec(1, b%M, ex) 
  }

  // primes
  def isPrime(j: Int): Boolean = primes takeWhile {x => x*x <= j} forall {j%_ != 0}
  def nextPrime(i: Int): Int = Stream.from(i+2, 2) find isPrime get
  val primes: Stream[Int] = 2 #:: 3 #:: primes.tail.map(nextPrime)

  def countDigits(p: Int): Int = p match {
    case 0 => 0
    case _ => 1 + countDigits(p/10)
  }

  // permutations
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
  def prevPermutation[T](src: Array[T])(implicit view: (T) => Ordered[T]): Boolean = {
    var i = src.length - 2
    while (i >= 0 && src(i) <= src(i+1)) i -= 1
    if (i < 0) return false
    var j = src.length - 1
    while (src(j) >= src(i)) j -= 1
    adjustArray(src, i, j) 
    true
  }

  // time
  def time[T](x : => T) =  {
    val start = System.nanoTime : Double
    val result = x
    val duration = (System.nanoTime : Double) - start
    println("Elapsed time " + duration / 1000000.0 + " msecs")
    result
  }
}
