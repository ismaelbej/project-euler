package ismael

object P31 {
  val weights = List(200, 100, 50, 20, 10, 5, 2, 1)
  val N = 200
  def countSums(s: Int, w: List[Int]): Int = w match {
    case Nil => if (s == 0) 1 else 0
    case h :: t => (0 to s/h map {x => countSums(s - x*h, t)}).sum
  }
  def run(args: Array[String]): Unit = {
    println(countSums(N, weights))
  }
}
