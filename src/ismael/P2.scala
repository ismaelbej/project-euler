package ismael

object P2 {
	val Max = 4000000
	val fib: Stream[Int] = 1 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
	val evenFib = fib takeWhile {_<Max} filter {_%2 == 0}
	def run(args: Array[String]): Unit = {
		println((0 /: evenFib) {_ + _})
	}
}
