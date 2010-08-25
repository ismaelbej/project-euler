package ismael

object P1 {
	def genList(N : Int) : List[Int] = List.range(1, N) filter {x => x%3==0 || x%5==0}

	def run(args: Array[String]): Unit = {
		println((0 /: genList(1000)) {_ + _})
	}
}

