package ismael

object P2 {	
	def genFibLessThan(N: Int): List[Int] = {
		def genFibSeq(a: Int, b:Int): List[Int] = {
			if (a > N) Nil
			else a :: genFibSeq(b, a+b)
		}
		genFibSeq(1, 1)
	}
	def filterSeq(seq: List[Int]): List[Int] = {
		for (n <- seq if n%2 == 0) yield n
	}
	def run(args: Array[String]): Unit = {
		println((0 /: filterSeq(genFibLessThan(4000000))) {_ + _})
	}
}