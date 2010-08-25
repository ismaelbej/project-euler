package ismael

object P3 {
	val Num: Long = 600851475143L
	def genDivisors(N: Long): List[Long] = {
		def removeDiv(n: Long, d: Long): Long = {
			if (n%d != 0) n
			else removeDiv(n/d, d)
		}
		def genDivFrom(n: Long, d: Long): List[Long] = {
			if (d > n) Nil
			else if (n%d == 0) d :: genDivFrom(removeDiv(n, d), d+1)
			else genDivFrom(n, d+1)
		}
		genDivFrom(N, 2)
	}
	def run(args: Array[String]): Unit = {
		println(genDivisors(Num) max)
	}
}

