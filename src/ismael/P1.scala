package ismael

object P1 {
  val N = 1000
  val filteredList = 1 to N - 1 filter {x => x % 3 == 0 || x % 5 == 0}
  def run(args: Array[String]): Unit = {
    println(filteredList sum)
  }
}
