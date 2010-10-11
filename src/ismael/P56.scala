package ismael

import ProjectEuler._

object P56 {
  val numbers = 2 to 99 reverse
  def sumDigits(n: BigInt): BigInt = {
    def sumDigitsRec(a: BigInt, b: BigInt): BigInt = {
      if (a == 0) b
      else sumDigitsRec(a / 10, b + a % 10)
    }
    sumDigitsRec(n, 0)
  }
  val powers = numbers flatMap {x => numbers map {y => power(BigInt(x), y)}}
  val sums = powers map sumDigits
  def run(args: Array[String]): Unit = {
    println(sums max)
  }
}
