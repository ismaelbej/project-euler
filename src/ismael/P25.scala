package ismael

object P25 {
	val digits = 1000
	def run(args: Array[String]): Unit = {
		var a = 1.0
		var b = 1.0
		var c = 1
		var n = 1
		while (c < digits) 
		{
			var t = b  
			b += a
			a = t
			if (a >= 10.0)
			{
				a /= 10.0
				b /= 10.0
				c += 1
			}
			n += 1
		}
		println(n)
	}
}