package ismael

object P16 {
	val number = power(2, 1000)
	def power(a: BigInt, n: Int): BigInt = {
		def powerRec(p: BigInt, n: Int): BigInt = {
			if (n==0) p
			else powerRec(p*a, n-1)
		}
		powerRec(1, n)
	}
	def sumDigits(n: BigInt): BigInt = {
		def sumDigitsRec(s: BigInt, n: BigInt): BigInt = {
			if (n == 0) s
			else sumDigitsRec(s+n%10, n/10)
		}
		sumDigitsRec(0, n)
	}
	def run(args: Array[String]): Unit = {
		println(sumDigits(number))
	}
}
