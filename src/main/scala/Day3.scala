import util.Day

object Day3 extends Day(3):
  override def solve(): Unit =
    val inputDigits = inputLines.map(_.map(_.asDigit).toList)

    //Part 1
    println(inputDigits.map(d => findJoltage(d, 2)).sum)

    //Part 2
    println(inputDigits.map(d => findJoltage(d, 12)).sum)

  def findJoltage(digits: List[Int], batteryCount: Int): Long =
    if batteryCount == 1 then
      digits.max
    else
      val d = digits.slice(0, digits.length - (batteryCount - 1)).max

      d * Math.pow(10L, batteryCount - 1).toLong + findJoltage(digits.slice(digits.indexOf(d) + 1, digits.length), batteryCount - 1)