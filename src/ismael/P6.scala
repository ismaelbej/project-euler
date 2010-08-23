package ismael

object P6 {
	val Num = 100;
	def sumInts(n: Int): Int = {
		n*(n+1)/2
	}
	def sumSquares(n: Int): Int = {
		n*(n+1)*(2*n+1)/6
	}
	def run(args: Array[String]): Unit = {
		val sum = sumInts(Num)
		println(sum * sum - sumSquares(Num))
	}
}