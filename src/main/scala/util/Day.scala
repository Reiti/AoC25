package util

import util.Util.Pos

trait Day(day: Int):
  lazy val input: String = Util.loadDay(day)
  lazy val rawInput: String = Util.loadDayKeepWhitespace(day)
  lazy val inputLines: List[String] = input.split("\n").toList
  lazy val inputInts: List[Int] = inputLines.map(_.toInt)
  lazy val inputMap: Map[Pos, Char] = inputLines.indices.flatMap(row => inputLines.head.indices.map(col => {
    (col, row) -> inputLines(row)(col)
  })).toMap

  def solve(): Unit

  def main(args: Array[String]): Unit = solve()