package ismael

object P10 {
	val N = 2000000
	val sieve = new Array[Boolean](N)
	def run(args: Array[String]): Unit = {
		var n: Int = 2
		while (n*n <= N)
		{
			if (!sieve(n))
			{
				for (k <- n to N/n if k*n < N) sieve(k*n) = true
			}
			n+=1
		}
		val primes = for (n <- 2 to N-1 if !sieve(n)) yield n
		println((BigInt(0) /: primes) {_+_})
	}
}