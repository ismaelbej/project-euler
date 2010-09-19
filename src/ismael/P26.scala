package ismael

import ProjectEuler._

object P26 {
  def multOrder(n: Int, d: Int): Int = {
    def multOrderRec(c: Int, p: Int): Int = p match {
      case 1 => c
      case _ => {
        val pn = ((p toLong) * n) % d
        multOrderRec(c + 1, pn toInt)
      }
    }
    if (dcm(n, d) > 1) 0
    else multOrderRec(1, n%d)
  }
  val M = 1000
  val oddsNonFiveMult = List.range(3, M, 2) filter {_%5!=0}
  val multOrders = oddsNonFiveMult map {x => (x, multOrder(10, x))}
  val maxOrder = multOrders.foldLeft((0, 0)) {(x, y) => if (y._2 > x._2) y else x}._1
  def run(args: Array[String]): Unit = {
    println(maxOrder)
  }
}
