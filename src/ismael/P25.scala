package ismael

object P25 {
	val digits = 1000
	def sumFib(p: ((Int, Double, Int), (Int, Double, Int))): (Int, Double, Int) = {
		val x = p._1
		val y = p._2
		if (x._1 == y._1) {
			val s = x._2 + y._2
			if (s >= 10.0) (x._1 + 1, s/10.0, y._3 + 1)
			else (x._1, s, y._3 + 1)
		}
		else (y._1, x._2/10.0 + y._2, y._3 + 1)
	}
	val fibDigits: Stream[(Int, Double, Int)] = (1, 1.0, 1) #:: (1, 1.0, 2) #:: fibDigits.zip(fibDigits.tail).map(sumFib)
	def run(args: Array[String]): Unit = {
		println((fibDigits dropWhile {_._1 < digits}).head._3)
	}
}
