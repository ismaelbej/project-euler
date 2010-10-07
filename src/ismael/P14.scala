package ismael

object P14 {
  val N = 1000000
  import scala.collection.mutable
  val values = mutable.Map.empty[Int, Int]
  def collatz(n: Int): Int = {
    def collatzRec(n: Long, c: Int): Int = {
      if (values.contains(n.toInt)) c + values(n.toInt)
      else {
	if (n == 1) c
	else if (n % 2 == 0) collatzRec(n / 2, c + 1)
	else collatzRec(3 * n + 1, c + 1)
      }
    }
    val r = collatzRec(n, 0)
    values + ((n, r))
    r
  }
  def run(args: Array[String]): Unit = {
    val s = (1 to N) map {x => (collatz(x), x)}
    val m = s.foldLeft((0, 0))((a, b) => {
      if (b._1 > a._1) b
      else a
    })
    println(m._2)
  }
}
