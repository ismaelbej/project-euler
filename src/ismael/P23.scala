package ismael

object P23 {
	def sumDiv(n: Int): Int = {
		def pow(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
		def getMaxPow(n: Int, d: Int): Int = {
			if (n%d == 0) 1+getMaxPow(n/d, d)
			else 0
		}
		def sumDivRec(n: Int, d: Int): Int = {
			if (n%d == 0)
			{
				val e = getMaxPow(n, d)
				val p = pow(d, e)
				val r = (p*d - 1)/(d - 1) 
				r * sumDivRec(n/pow(d, e), d+1)
			}
			else if (d*d > n)
			{
				if (n>1) n+1
				else 1
			}
			else sumDivRec(n, d+1)
		}
		sumDivRec(n, 2)
	}
	def isAbundant(n: Int): Boolean = sumDiv(n) > 2*n
	val Max = 28123
	val abundantNumbers = for (n <- 1 to Max if isAbundant(n)) yield n
	def makeSumTwoAbundants(): Set[Int] = {
		(for (n <- abundantNumbers; m <- abundantNumbers dropWhile {_<n} if n+m <= Max) yield m+n) toSet
	}
	def run(args: Array[String]): Unit = {
		val s = makeSumTwoAbundants.sum
		println(Max*(Max+1)/2 - s)
	}
}
