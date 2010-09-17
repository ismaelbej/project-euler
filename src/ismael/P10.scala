package ismael

object P10 {
  val N = 2000000
  val primesBelowN = ProjectEuler.primes takeWhile {_<=N} map {_.toLong}
  def run(args: Array[String]): Unit = {
    println(primesBelowN sum)
  }
}
