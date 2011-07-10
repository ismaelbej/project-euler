package ismael

object P18 {
  val pyramidString = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""
  def makeList(s: String): List[Int] = s split(' ') map {_ toInt} toList
  def makePyramid(s: String): List[List[Int]] = s split('\n') map makeList toList
  val pyramid = makePyramid(pyramidString)
  def makeMaxRow(t: List[Int]): List[Int] = {
    0 :: t zip t ::: List(0) map {x => x._1 max x._2}
  }
  def addRows(t: List[Int], b: List[Int]): List[Int] = {
    makeMaxRow(t) zip b map {x => x._1 + x._2}
  }
  def findMaxPyramid(a: List[List[Int]]): Int = {
    def addPyramidRows(q: List[Int], a: List[List[Int]]): List[Int] = a match {
      case h :: t => addPyramidRows(addRows(q, h), t)
      case _ => q
    }
    addPyramidRows(a head, a tail) max
  }
  def run(args: Array[String]): Unit = {
    println(findMaxPyramid(pyramid))
  }
}
