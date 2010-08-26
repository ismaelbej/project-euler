package ismael

object P21 {
	val N = 10000
	def sumDiv(n: Int): Int = {
		def sumDiv_tr(s: Int, d: Int): Int = {
			if (d*d>n) s
			else if (d*d==n) s+d
			else if (n%d==0) sumDiv_tr(s+n/d+d, d+1)
			else sumDiv_tr(s, d+1)
		}
		1+sumDiv_tr(0, 2)
	}
	val sums = List.range(1, 3*N) map {sumDiv _}
	def isAmicable(n: Int): Boolean = {
		val m = sums(n-1) 
		if (n == m) false
		else sums(m-1) == n
	}

	def run(args: Array[String]): Unit = {
		val amicables = List.range(1, N) filter isAmicable
		println((0 /: amicables) {_+_})
	}
}