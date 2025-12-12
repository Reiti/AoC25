import util.Day
import util.Util.{x, y}

import scala.annotation.tailrec

object Day12 extends Day(12):
  override def solve(): Unit =
    val shapes = parseShapes(inputLines.takeWhile(l => !l.contains('x')))
    val boxes = inputLines.dropWhile(l => !l.contains('x')).map(_.trim).filter(l => !l.isBlank).map(b => b.split(":")).map(b => (b.head.trim.split("x"), b.last.trim.split(" "))).map(b => ((b._1.head.toInt, b._1.last.toInt), b._2.map(_.toInt)))

    //Part 1
    println(boxes.count(b => (b._1.x * b._1.y) >= b._2.zip(shapes.map(_.mkString.count(_ == '#'))).map(z => z._1 * z._2).sum)) //lol, lmao even

  @tailrec
  def parseShapes(input: List[String], shapes: List[List[String]] = List()): List[List[String]] =
      val relevant = input.dropWhile(s => s.isBlank || s.charAt(0).isDigit)

      if relevant.isEmpty then
        shapes
      else
        parseShapes(relevant.dropWhile(s => s.startsWith("#") || s.startsWith(".")), shapes.appended(relevant.takeWhile(s => s.startsWith("#") || s.startsWith("."))))