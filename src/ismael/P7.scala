package ismael

object P7 {
	def isPrime(j: Int): Boolean = primes takeWhile {x => x*x <= j} forall {j%_!=0}
	def nextPrime(i: Int): Int = Stream.from(i + 1).find(isPrime).get
	val primes: Stream[Int] = 2 #:: primes.map(nextPrime)
	def run(args: Array[String]): Unit = {
		println(primes take 10001 last)
	}
}
