import util.Day

import scala.annotation.tailrec

object Day2 extends Day(2):
  override def solve(): Unit =
    val ranges = input.split(",").map(_.split("-")).map(l => l.head.toLong.to(l.last.toLong))

    //Part 1
    println(ranges.flatMap(r => r.map(_.toString).filter(s => s.length % 2 == 0 && s.substring(0, s.length/2).equals(s.substring(s.length/2)))).map(_.toLong).sum)

    //Part 2
    println(ranges.flatMap(r => r.map(_.toString).filter(s => isInvalid(s))).map(_.toLong).sum)

  @tailrec
  def isInvalid(id: String, subLength: Int = 1): Boolean =
    if subLength > id.length / 2 then
      false
    else if id.length % subLength == 0 && id == id.substring(0, subLength) * (id.length / subLength) then
      true
    else
        isInvalid(id, subLength + 1)