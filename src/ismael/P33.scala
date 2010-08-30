package ismael

object P33 {
	def isEquivFrac(a: Int, b: Int, d: Int): Boolean = {
		val ab = 10*a+b
		val bd = 10*b+d
		ab*d == bd*a
	}
	def isNonTrivialFrac(a: Int, d:Int): Boolean = {
		!(for (b <- 1 to 9 if isEquivFrac(a, b, d)) yield b).isEmpty
	}
	def dcm(n: Int, d: Int) : Int = {
		if (d==0) n
		else dcm(d, n%d)
	}
	def run(args: Array[String]): Unit = {
		val frac = for (a <- 1 to 8; d <- a+1 to 9 if isNonTrivialFrac(a, d)) yield (a,d)
		val (num, denom) = ((1,1) /: frac) {(x, y) => (x._1* y._1, x._2*y._2)}
		val d = dcm(num, denom)
		println(denom/d)
	}
}