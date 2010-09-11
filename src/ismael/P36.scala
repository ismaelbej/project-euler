package ismael

object P36 {
	def reverse(n: Int, b: Int): Int = {
		def reverseRec(r: Int, n:Int): Int = n match {
			case 0 => r
			case _ => reverseRec(r*b+n%b, n/b)
		}
		reverseRec(0, n)
	}
	def isPalindromicBase(n: Int, b: Int): Boolean = n == reverse(n, b)
	def isPalindromic(n: Int): Boolean = {
		isPalindromicBase(n, 2) && isPalindromicBase(n, 10)
	}
	val Max = 1000000
	val palindromics = List.range(1, Max, 2) filter isPalindromic
	def run(args: Array[String]): Unit = {
		println(palindromics sum)
	}
}
