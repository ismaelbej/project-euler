package ismael

object P53 {
  val Max = 1000000L
  def findFirstExceeding(n: Long): Long = {
    def findFirstRec(p: Long, c: Long): Long = {
      if (c > n / 2) -1
      else if (p > Max) c
      else findFirstRec(p * (n - c) / (c + 1), c + 1)
    }
    findFirstRec(n, 1)
  }
  def countExceeding(n: Long): Long = {
    val p = findFirstExceeding(n)
    if (p < 0) 0
    else 1 + n - 2 * p 
  }
  def run(args: Array[String]): Unit = { 
    val exceedings = 1L to 100L map countExceeding
    println(exceedings sum)
  }
}
