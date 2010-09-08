package ismael

object P29 {
	val MaxBase = 100
	val MaxExp = 100
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
		val t = bases takeWhile(x => x*x <= n)
		t.forall(x => !isPowerOf(x))
	}
	def nextNonPower(n: Int): Int = Stream.from(n+1).find(isNotPower).get 
	val bases: Stream[Int] = 2 #:: bases.map(nextNonPower)
	def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
	def countForBase(b: Int): Int =	{
		def genExponents(n: Int) = List.range(2*n, MaxExp*n+1, n)
		val exps = Stream.from(1).takeWhile(x => power(b, x) <= MaxBase).flatMap(genExponents)
		(exps toSet).size
	}
	def run(args: Array[String]): Unit = {
		val s = bases takeWhile(_<=MaxBase)
		val count = (s map countForBase).sum
		println(count)
	}
}
