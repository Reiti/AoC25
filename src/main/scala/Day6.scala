import util.Day

import scala.annotation.tailrec

object Day6 extends Day(6):
  override def solve(): Unit =
    //Part 1
    println(inputLines.map(_.split("\\s+").map(_.trim)).transpose.map(l => l.init.map(_.toLong).reduce(operator(l.last.trim.head))).sum)

    val l = inputLines.map(_.padTo(inputLines.head.length, ' ')).transpose

    //Part 2
    println(partition(l).map(_.map(_.trim)).map(l => l.tail.prepended(l.head.init.trim).map(_.toLong).reduce(operator(l.head.last))).sum)

  def operator(l: Char): (Long, Long) => Long = if l == '+' then _ + _ else _ * _

  @tailrec
  def partition(list: List[List[Char]], groups: List[List[String]] = List()): List[List[String]] =
    if list.isEmpty then
      groups
    else
      val g = list.takeWhile(p => !p.forall(_ == ' '))
      val rest = list.drop(g.length + 1)

      partition(rest, groups.appended(g.map(_.mkString)))


