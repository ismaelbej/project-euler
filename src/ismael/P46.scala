package ismael

object P46 {
	def isPrime(j: Int): Boolean = primes.takeWhile(x => x*x<=j).forall(j%_!=0)
	def nextPrime(i: Int): Int = Stream.from(i + 1).find(isPrime).get
	val primes: Stream[Int] = 2 #:: primes.map(nextPrime)
	val odds = Stream.from(0) map {2*_ + 1}
	val squares = Stream.from(0) map {x => x*x}
	def isGoldbach(n: Int): Boolean = squares.takeWhile(y => 2*y < n).exists(y => isPrime(n - 2*y))
	val nonGoldbach = odds dropWhile isGoldbach
	def run(args: Array[String]): Unit = {
		println(nonGoldbach head)
	}
}
