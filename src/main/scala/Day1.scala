import util.Day

import scala.annotation.tailrec

object Day1 extends Day(1):
  override def solve(): Unit =
    val dialPositions = inputLines.scanLeft(50)(turnDial)

    //Part 1
    println(dialPositions.count(_ == 0))

    //Part 2
    println(dialPositions.zip(inputLines).map(l => countZeroes(l.head,  l.last)).sum)

  def turnDial(pos: Int, rot: String): Int = rot match
    case s"L${dist}" => Math.floorMod(pos - dist.toInt, 100)
    case s"R${dist}" => Math.floorMod(pos + dist.toInt, 100)

  def countZeroes(pos: Int, rot: String): Int =
      val (fullRotations, newPos) = rot match
        case s"L${dist}" => (dist.toInt / 100, pos - (dist.toInt % 100))
        case s"R${dist}" => (dist.toInt / 100, pos + (dist.toInt % 100))

      if pos != 0 && (newPos <= 0 || newPos >= 100) then
        fullRotations + 1
      else
        fullRotations