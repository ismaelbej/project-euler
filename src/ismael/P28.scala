package ismael

object P28 {
	val odds = List.range(3, 1002, 2)
	def sumVert(n: Int): Int = 4*n*n - 6*(n-1) 
	def run(args: Array[String]): Unit = {
		val s = for (n <- odds) yield sumVert(n) 
		println(s.foldLeft(1)(_+_))
	}
}
