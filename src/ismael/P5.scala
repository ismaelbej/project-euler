package ismael

object P5 {
	def dcm(n: Int, d: Int) : Int = {
		if (d==0) n
		else dcm(d, n%d)
	}
	def mcm(n: Int, d: Int) : Int = {
		n / dcm(n, d) * d
	}
	def genDivisibleBetween(from: Int, to: Int): Int = {
		def makeDivisible(n: Int, d: Int): Int = {
			if (d < from) n
			else makeDivisible(mcm(n, d), d-1)
		}
		makeDivisible(1, to)
	}
	def run(args: Array[String]): Unit = {
		println(genDivisibleBetween(1, 20))
	}
}