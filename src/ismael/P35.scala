package ismael

object P35 {
	def sq(j: Int)(k: Int): Boolean = k*k <= j
	def isPrime(j: Int): Boolean = {
		if (j >= 0) primes.takeWhile(sq(j)).forall(j%_!=0)
		else primes.takeWhile(sq(-j)).forall(j%_!=0)
	}
	def nextPrime(i: Int): Int = Stream.from(i + 1).find(isPrime).get
	val primes: Stream[Int] = 2 #:: primes.map(nextPrime)
	val Max = 1000000
	def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
	def isCircular(p: Int): Boolean = {
		val l = countDigits(p)
		def rotateLeft(d: Int): Int = {
			val b = power(10,l-d)
			val c = power(10,d)
			val a = p/b
			(p-a*b)*c+a
		}
		(for (n <- 1 to l) yield rotateLeft(n)).forall(isPrime)
	}
	def countDigits(p: Int): Int = p match {
		case 0 => 0
		case _ => 1 + countDigits(p/10)
	}
	val circularPrimes = primes takeWhile {_<Max} filter isCircular
	def run(args: Array[String]): Unit = {
		println(circularPrimes.length)
	}
}
