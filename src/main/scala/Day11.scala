import util.{Day, Util}

object Day11 extends Day(11):
  override def solve(): Unit =
    val edges = inputLines.map {case s"${from}: ${to}" => (from, to.split(" ").toList)}.map(e => (e._1 -> e._2)).toMap

    //Part 1
    println(countPaths(edges, "you", "out"))

    //Part 2
    println(countPaths(edges, "svr", "fft") * countPaths(edges, "fft", "dac") * countPaths(edges, "dac", "out"))

  def countPaths(edges: Map[String, List[String]], from: String, to: String): Long =
    lazy val count: String => Long = Util.memoize:
      case s if s == to => 1
      case other => edges.getOrElse(other, List()).map(count).sum

    count(from)