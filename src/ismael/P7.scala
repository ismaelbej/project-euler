package ismael

object P7 {
	def span(p: Int => Boolean)(s: Stream[Int]): (Stream[Int], Stream[Int]) = {
		(s.takeWhile(p), s.dropWhile(p)) 
	}
	def sieve(pr: Stream[Int], s: Stream[Int]): Stream[Int] = {
		val p = pr.head 
		val (h, t) = span({_<p*p})(s)
		h.append(sieve(pr.tail, t.tail filter {_%p != 0}))
	}
	val odds: Stream[Int] = Stream.cons(1, odds map {_+2})
	val primes: Stream[Int] = 2 #:: 3 #:: sieve(primes tail, odds drop 2)
	
	def run(args: Array[String]): Unit = {
		println(primes take 10001 last)
	}
}