package ismael

object P25 {
  val digits = 1000
  def makeFibonacci(p: ((Int, Double), (Int, Double))): (Int, Double) = p match {
    case (x, y) => {
      if (x._1 == y._1) {
        val s = x._2 + y._2
        if (s >= 10.0) (x._1 + 1, s / 10.0)
        else (x._1, s)
      }
      else (y._1, x._2 / 10.0 + y._2)
    }
  }
  val fibonacciWithDigits: Stream[(Int, Double)] = (1, 1.0) #:: (1, 1.0) #:: fibonacciWithDigits.zip(fibonacciWithDigits.tail).map(makeFibonacci)
  val fibonacciDigits = fibonacciWithDigits map {_._1}
  val count = 1 + (fibonacciDigits takeWhile {_ < digits} size)
  def run(args: Array[String]): Unit = {
    println(count)
  }
}
