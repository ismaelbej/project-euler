package ismael

object P27 {
	def sq(j: Int)(k: Int): Boolean = k*k <= j
	def isPrime(j: Int): Boolean = {
		if (j >= 0) primes.takeWhile(sq(j)).forall(j%_!=0)
		else primes.takeWhile(sq(-j)).forall(j%_!=0)
	}
	def nextPrime(i: Int): Int = Stream.from(i + 1).find(isPrime).get
	val primes: Stream[Int] = 2 #:: primes.map(nextPrime)
	def poly(a: Int, b: Int)(n: Int): Int = n*n + a*n + b
	def run(args: Array[String]): Unit = {
		val s = for (b <- primes takeWhile {_<1000}; a <- -999 to 999 if isPrime(1+a+b)) yield (a*b, (Stream.from(0) takeWhile {x => isPrime(poly(a, b)(x))}).length)
		val t = for (b <- primes takeWhile {_<1000}; a <- -999 to 999 if isPrime(1+a-b)) yield (-a*b, (Stream.from(0) takeWhile {x => isPrime(poly(a, -b)(x))}).length)
		val m = s.foldLeft((0, 0))((a, b) => {
			if (b._2 > a._2) b
			else a
		})
		val n = t.foldLeft((0, 0))((a, b) => {
			if (b._2 > a._2) b
			else a
		})
		if (n._2 > m._2) println(n._1)
		else println(m._1)
	}
}
