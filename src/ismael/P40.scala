package ismael

object P40 {
	def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_} 
	def makeDigits(n: Int): Int = n match {
		case 0 => 0
		case _ => n * (power(10, n) - power(10, n-1))
	}
	val cantDigits = Stream.from(1) map makeDigits
	def findPos(n: Int): (Int, Int) = {
		def findPosRec(n: Int, s: Stream[(Int, Int)]): (Int, Int) = {
			val h = s.head
			if (n <= h._1) (h._2+1, n)
			else findPosRec(n - h._1, s.tail)
		}
		findPosRec(n, cantDigits zipWithIndex)
	}
	def findDigit(n: Int): Int = {
		val (d, p) = findPos(n)
		val b = (p-1)/d
		val r = d-1-(p-1)%d
		val m = power(10, d-1)+b
		(m / power(10, r))%10
	}
	def run(args: Array[String]): Unit = {
		val digits = List(1, 10, 100, 1000, 10000, 100000, 1000000) map findDigit
		println((1 /: digits) {_*_})
	}
}
