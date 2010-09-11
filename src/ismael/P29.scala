package ismael

object P29 {
	val MaxBase = 100
	val MaxExp = 100
	def square(n: Int): Int = n*n
	def isNotPower(n: Int): Boolean = {
		def isPowerOf(b: Int): Boolean = {
			def isPowerOfRec(a: Int): Boolean = {
				if (a > n) false
				else if (a == n) true
				else if (n%a != 0) false
				else isPowerOfRec(a*b)
			}
			isPowerOfRec(b*b)
		}
		bases takeWhile {square(_) <= n} forall {!isPowerOf(_)}
	}
	def nextNonPower(n: Int): Int = Stream.from(n+1) find isNotPower get 
	val bases: Stream[Int] = 2 #:: bases.map(nextNonPower)
	def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
	def countForBase(b: Int): Int =	{
		def genExponents(n: Int) = List.range(2*n, MaxExp*n+1, n)
		(Stream.from(1) takeWhile {power(b, _) <= MaxBase} flatMap genExponents toSet).size
	}
	def run(args: Array[String]): Unit = {
		val s = bases takeWhile {_<=MaxBase} map countForBase
		println(s sum)
	}
}
