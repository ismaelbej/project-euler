package ismael

object P24 {
	val digits = List.range(0, 10)
	def fact(n: Int): Int = (1 /: List.range(1, n+1)) {_*_}
	def calcNumber(position: Int): Long = {
		def calcDigits(p: Int, d: List[Int]): List[Int] = {
			if (d.isEmpty) List.empty
			else
			{
				val l = d.length-1
				val f = fact(l)
				val n = p/f
				val r = p%f
				d(n) :: calcDigits(r, d filter {_ != d(n)})
			}
		}
		def makeNumber(l: List[Int]): Long = {
			def makeNumberRec(a: Long, b: List[Int]): Long = {
				if (b.isEmpty) a
				else makeNumberRec(10*a+b.head, b.tail)
			}
			makeNumberRec(0, l)
		}
		makeNumber(calcDigits(position, digits))
	}
	def run(args: Array[String]): Unit = {
		println(calcNumber(999999))
	}
}