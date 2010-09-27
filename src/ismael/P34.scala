package ismael

object P34 {
  def fact(b: Int): Int = (1 /: List.range(1, b + 1)) {_ * _}
  def getFactSum(l: List[Int]): Int = l.foldLeft(0)((x, y) => x + fact(y))
  def getDigits(n: Int): List[Int] = n match {
    case 0 => Nil
    case _ => n % 10 :: getDigits(n / 10)
  }
  def hasDigits(n: Int): List[Int] = getDigits(n) sortWith {_ > _}
  def isFactSum(l: List[Int]): Boolean = hasDigits(getFactSum(l)) == l
  def nextSequenceIncr(l: List[Int]): List[Int] = l match {
    case Nil => List(0)
    case _ => nextSequence(l)
  }
  def nextSequence(l: List[Int]): List[Int] = l match {
    case h :: t => {
      if (h < 9) (h+1) :: t
      else {
	val r = nextSequenceIncr(t)
	r.head :: r
      }
    }
    case _ => Nil
  }
  val sequences: Stream[List[Int]] = List(0, 1) #:: sequences.map(nextSequence)
  val validNumbers = sequences takeWhile(_.length <= 7) filter isFactSum map getFactSum
  def run(args: Array[String]): Unit = {
    println(validNumbers.sum)
  }
}
