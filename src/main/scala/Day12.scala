import util.Day

object Day12 extends Day(12):
  override def solve(): Unit =
    //Part 1
    println(inputLines.dropWhile(l => !l.contains('x')).map(b => b.split(":")).count(b => b.head.trim.split("x").map(_.toInt).product >= b.last.trim.split(" ").map(_.toInt).sum * 9)) //lol, lmao even