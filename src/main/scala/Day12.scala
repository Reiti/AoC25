import util.Day
import util.Util.{x, y}

object Day12 extends Day(12):
  override def solve(): Unit =
    val boxes = inputLines.dropWhile(l => !l.contains('x')).map(_.trim).filter(l => !l.isBlank).map(b => b.split(":")).map(b => (b.head.trim.split("x"), b.last.trim.split(" "))).map(b => ((b._1.head.toInt, b._1.last.toInt), b._2.map(_.toInt)))

    //Part 1
    println(boxes.count(b => (b._1.x * b._1.y) >= b._2.map(_ * 9).sum)) //lol, lmao even