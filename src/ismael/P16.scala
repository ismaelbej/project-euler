package ismael

object P16 {
	val number = power(BigInt(2), 1000)
	def power(a: BigInt, n: Int): BigInt = (BigInt(1) /: List.fill(n)(a)) {_*_}
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
