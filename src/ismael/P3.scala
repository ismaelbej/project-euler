package ismael

object P3 {
	val Num:BigInt = BigInt("600851475143")
	def genDivisors(N: BigInt): List[BigInt] = {
		def removeDiv(n: BigInt, d: BigInt): BigInt = {
			if (n%d != 0) n
			else removeDiv(n/d, d)
		}
		def genDivFrom(n: BigInt, d: BigInt): List[BigInt] = {
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