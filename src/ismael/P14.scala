package ismael


object P14 {
	val N = 1000000
	import scala.collection.mutable
	val vals = mutable.Map.empty[Long, Long]
	def collatz(n: Long): Long = {
		def collatzRec(n: Long, c: Long): Long = {
			if (vals.contains(n)) c+vals(n)
			else
			{
				if (n == 1) c
				else if (n%2 == 0) collatzRec(n/2, c+1)
				else collatzRec(3*n+1,c+1)
			}
		}
		val r = collatzRec(n, 0)
		vals + ((n, r))
		r
	}
	def run(args: Array[String]): Unit = {
		var max = 0L
		var pos = 0
		for (x <- 1 to N) 
		{
			if (collatz(x) > max)
			{
				max = collatz(x)
				pos = x
			}
		}
		println(pos)
	}
}
