package ismael

object P41 {
	def adjustArray[T](src:Array[T],i:Int,j:Int){
		var tmp = src(i)
        src(i) = src(j)
        src(j) = tmp
        var len = (src.length - i)/2
        for(k <- 1 to len){
            tmp = src(src.length - k)
            src(src.length - k) = src(i + k)
            src(i + k) = tmp
        }
	}
    def prevPermutation[T](src:Array[T])
                    (implicit view:(T) => Ordered[T]):Boolean = {
        var i = src.length - 2
        while(i >= 0 && src(i) <= src(i+1)) i -= 1
        if(i < 0) return false
        var j = src.length - 1
        while(src(j) >= src(i)) j -= 1
        adjustArray(src,i,j) 
        true
    }
    def prevSequence(l: List[Int]): List[Int] = {
    	var r: Array[Int] = l.toArray
		prevPermutation(r)
    	r toList
	}
	def isPrime(j: Int): Boolean = primes takeWhile {x => x*x <= j} forall {j%_!=0}
	def nextPrime(i: Int): Int = Stream.from(i + 1).find(isPrime).get
	val primes: Stream[Int] = 2 #:: primes.map(nextPrime)
	def makeNumber(l: List[Int]): Int = {
		def makeNumberRec(n: Int, l: List[Int]): Int = l match {
			case Nil => n
			case h :: t => makeNumberRec(10*n + h, t)
		}
		makeNumberRec(0, l)
	}
	val pandigitalSequences: Stream[List[Int]] = List(7, 6, 5, 4, 3, 2, 1) #:: pandigitalSequences.map(prevSequence)
	val pandigital = pandigitalSequences map makeNumber
	def run(args: Array[String]): Unit = {
		
		println(pandigital dropWhile {x => !isPrime(x)} head)
	}
}
