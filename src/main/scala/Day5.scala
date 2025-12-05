import util.Day

import scala.annotation.tailrec

object Day5 extends Day(5):
  override def solve(): Unit =
    val split = input.split("\n\n")
    val ranges = split.head.split("\n").map(_.trim).map{case s"${from}-${to}" => (from.toLong, to.toLong)}
    val ingredients = split.last.split("\n").map(_.trim).map(_.toLong)

    //Part 1
    println(ingredients.count(i => isFresh(ranges, i)))

    //Part 2
    println(merge(ranges).map(r => r._2 - r._1 + 1).sum)

  def isFresh(ranges: Array[(Long, Long)], id: Long): Boolean = ranges.exists(r => r._1 <= id && r._2 >= id)

  @tailrec
  def merge(ranges: Array[(Long, Long)]): Array[(Long, Long)] =
    ranges.combinations(2).find(c => union(c.head, c.last).length == 1) match
      case Some(c) =>
        merge(ranges.diff(c).appended(union(c.head, c.last).head))
      case _ => ranges

  def union(a: (Long, Long), b: (Long, Long)): Array[(Long, Long)] =
    if a._1 > b._2 || a._2 < b._1 then
      Array(a, b)
    else if a._1 >= b._1 then
      if a._2 <= b._2 then
        Array(b)
      else
        Array((b._1, a._2))
    else
      if a._2 <= b._2 then
        Array((a._1, b._2))
      else
        Array(a)