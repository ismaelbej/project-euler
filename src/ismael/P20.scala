package ismael

object P20 {
	val N = 100
	def fact(n: BigInt): BigInt = {
		def fact_tr(p: BigInt, n: BigInt): BigInt = {
			if (n <= 1) p
			else fact_tr(p*n, n-1)
		}
		fact_tr(1, n)
	}
	def sum_digits(n: BigInt): BigInt = {
		def sum_tr(s: BigInt, n: BigInt): BigInt = {
			if (n == 0) s
			else sum_tr(s+n%10, n/10)
		}
		sum_tr(0, n)
	}

	def run(args: Array[String]): Unit = {
		val h = fact(N)
		println(sum_digits(h))
	}
}