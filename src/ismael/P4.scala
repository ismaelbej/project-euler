package ismael

object P4 {
	def genThreeDigitDivisor(N: Int): List[Int] = {
		for (d <- List.range(100, 1000) if (N%d == 0 && N/d >= 100 && N/d < 1000)) yield d
	}
	def isThreeDigitProduct(N: Int): Boolean = {
		! (genThreeDigitDivisor(N) isEmpty)
	}
	def makeReverse(n: Int): Int = {
		100 * (n%10) + 10 * ((n/10)%10) + n/100
	}
	def makePalindrome(n: Int): Int = {
		return n * 1000 + makeReverse(n)
	}
	val palindromes: List[Int] = List.range(999, 100, -1) map {makePalindrome(_)}
	val threeDigitProduct = palindromes filter {isThreeDigitProduct(_)}  
	def run(args: Array[String]): Unit = {
		println(threeDigitProduct head)
	}
}
