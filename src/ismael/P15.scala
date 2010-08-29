package ismael

object P15 {
	def prod(a: Int, b: Int): BigInt = (BigInt(1) /: List.range(a, b+1)) {_*_}
	def comb(n: Int, d: Int): BigInt = prod(d+1, n)/prod(1, d)
	
	def run(args: Array[String]): Unit = {
		println(comb(40, 20))
	}
}
