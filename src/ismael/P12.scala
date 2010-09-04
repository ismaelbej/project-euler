package ismael

object P12 {
	def power(a: Int, n: Int): Int = (1 /: List.fill(n)(a)) {_*_}
	def countMaxDiv(n: Int, d: Int): Int = {
		def countMaxDivRec(n: Int, c: Int): Int = {
			if (n%d != 0) c
			else countMaxDivRec(n/d, c+1)
		}
		countMaxDivRec(n, 0)
	}
	def countDiv(n: Int): Int = {
		def countDivRec(n: Int, p: Int, d: Int): Int = {
			if (n == 1) p
			else if (d*d > n) 2*p
			else
			{
				val k = countMaxDiv(n, d)
				val m = power(d, k)
				countDivRec(n/m, p*(k+1), d+1)
			}
		}
		countDivRec(n, 1, 2)
	}
	def triangleNumber(n: Int): Int = n*(n+1)/2
	def makeTriangleNumbers(s: Stream[Int]): Stream[Int] = Stream.cons(triangleNumber(s head), makeTriangleNumbers(s tail));
	val naturalNumbers: Stream[Int] = Stream.cons(1, naturalNumbers map {_+1}) 
	val triangleNumbers = Stream.cons(1, makeTriangleNumbers(naturalNumbers tail))
	def run(args: Array[String]): Unit = {
		println(triangleNumbers filter {countDiv(_) > 500} head)
	}
}
