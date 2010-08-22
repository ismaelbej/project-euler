package ismael

object P1 {
	def genList(N : Int) : List[Int] = {
		for (n <- List.range(1, N) if n%3==0 || n%5==0) yield n 
	}

	def run(args: Array[String]): Unit = {
		println((0 /: genList(1000)) {(x, y) => x + y})
	}
}