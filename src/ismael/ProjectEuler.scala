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
  def isPrime[T](j: T)(implicit integral: Integral[T]): Boolean = {
    def square(x: Int): T = integral.times(integral.fromInt(x), integral.fromInt(x))
    if (j == integral.one) false
    else primes takeWhile {x => integral.lteq(square(x), j)} forall {x => integral.rem(j, integral.fromInt(x)) != 0}
  }
  def nextPrime(i: Int): Int = Stream.from(i+2, 2) find isPrime get
  val primes: Stream[Int] = 2 #:: 3 #:: primes.tail.map(nextPrime)

  def factor(n: Int): List[(Int, Int)] = {
    def reduceDiv(c: Int, m: Int, d: Int): (Int, Int) = {
      if (m % d != 0) (c, m)
      else reduceDiv(c + 1, m / d, d)
    }
    def factorize(n: Int, d: Stream[Int]): List[(Int, Int)] = {
      val h = d.head
      if (n == 1) Nil
      else if (h*h > n) List((n, 1))
      else {
        val (c, m) = reduceDiv(0, n, h)
        if (c > 0) (h, c) :: factorize(m, d.tail)
        else factorize(n, d.tail)
      }
    }
    factorize(n, primes)
  }

  // contar digitos
  def countDigits[T](n: T)(implicit integral: Integral[T]): Int = n match {
    case 0 => 0
    case _ => 1 + countDigits(integral.quot(n, integral.fromInt(10)))
  }

  // divisor comun minimo
  def dcm[T](a: T, b: T)(implicit integral: Integral[T]): T = b match {
    case 0 => a
    case _ => dcm(b, integral.rem(a, b))
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
