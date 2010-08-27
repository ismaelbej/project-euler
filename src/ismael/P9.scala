package ismael

object P9 {
	val N = 1000
	val findPitagoras = {
		for (a <- 1 to N; b <- a+1 to N-a; c=N-a-b if a*a + b*b == c*c) yield (a, b, c) 
	}
	def run(args: Array[String]): Unit = {
		val p = findPitagoras(0)
		println(p._1 * p._2 * p._3)
	}
}