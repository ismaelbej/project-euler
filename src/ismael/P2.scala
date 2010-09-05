package ismael

object P2 {	
	def genFibLessThan(N: Int): List[Int] = {
		def genFibSeq(a: Int, b:Int): List[Int] = {
			if (a > N) Nil
			else a :: genFibSeq(b, a+b)
		}
		genFibSeq(1, 1)
	}
	val Max = 4000000
	val evenFib = genFibLessThan(Max) filter {_%2 == 0}
	def run(args: Array[String]): Unit = {
		println((0 /: evenFib) {_ + _})
	}
}
