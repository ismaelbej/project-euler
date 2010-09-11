package ismael

object P30 {
	val ex = 5
	def power(b: Int, e: Int): Int = (1 /: List.fill(e)(b)) {_*_}
	def getPowerSum(l: List[Int]): Int = l.foldLeft(0)((x, y) => x+power(y, ex))
	def getDigits(n: Int): List[Int] = n match {
		case 0 => Nil
		case _ => n%10 :: getDigits(n/10)
	}
	def hasDigits(n: Int, l: List[Int]): Boolean = getDigits(n).filter(_!=0).sort(_>_) == l
	def isPowerSum(l: List[Int]): Boolean = hasDigits(getPowerSum(l), l)
	def nextSequenceIncr(l: List[Int]): List[Int] = l match {
		case Nil => List(1)
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
	val sequences: Stream[List[Int]] = List(1, 1) #:: sequences.map(nextSequence)
	val validNumbers = sequences takeWhile(_.length <= 6) filter isPowerSum map getPowerSum
	def run(args: Array[String]): Unit = {
		println(validNumbers.foldLeft(0) {_+_})
	}
}
