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
  def makePyramid(s: String): Array[Array[Int]] = s.split('\n') map {_.split(' ') map {_.toInt}}
  val pyramid = makePyramid(pyramidString)
  def sumPos(t: Array[Int], b: Array[Int], n: Int): Int = {
    if (n == 1) b(n - 1) + t(0)
    else if (n == b.length) b(n - 1) + t(n - 2)
    else b(n - 1) + List(t(n - 2), t(n - 1)).max
  }
  def addRows(t: Array[Int], b: Array[Int]): Array[Int] = {
    (for (n <- 1 to b.length) yield sumPos(t, b, n)) toArray
  }
  def findMaxPyramid(a: Array[Array[Int]]): Int = {
    def addPyramidRows(h: Array[Int], a: Array[Array[Int]]): Array[Int] = {
      if (a.isEmpty) h
      else addPyramidRows(addRows(h, a.head), a.tail)
    }
    addPyramidRows(a head, a tail) max
  }
  def run(args: Array[String]): Unit = {
    println(findMaxPyramid(pyramid))
  }
}
