package util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Util {
  def loadDay(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayKeepWhitespace(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString
    }.get
  }

  def loadFilename(name: String): String = {
    Using(Source.fromResource(name)) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayLines(day: Int): List[String] = loadDay(day).split("\n").toList

  def parseMap(lines: Seq[String]): Map[Pos, Char] =
    lines.indices.flatMap(row => lines.head.indices.map(col => {
      (col, row) -> lines(row)(col)
    })).toMap

  @tailrec
  def gcd(a: Long, b: Long): Long = {
    if(b == 0)
      a
    else
      gcd(b, a%b)
  }

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def lcm(nums: Long*): Long = nums.reduce(lcm)

  def zip3[A](a: List[A], b: List[A], c: List[A]): List[(A, A, A)] = {
    (a zip b zip c) map {
      case ((a, b), c) => (a, b, c)
    }
  }

  def zip3[A](t: (List[A], List[A], List[A])): List[(A, A, A)] = zip3(t._1, t._2, t._3)

  def time[A](block: => A): A = {
    val t0  = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  private[Util] def getAnyPath[B](map: Map[B, Set[B]], start: B): List[B] = {
    @tailrec
    def getPathH(curr: B, acc: List[B]): List[B] = map.get(curr) match {
      case Some(n) => getPathH(n.head, curr :: acc)
      case None => curr :: acc
    }
    getPathH(start, List())
  }

  case class AStarResult[B](distance: Int, path: List[B])
  private[Util] def _astar[B](start: B, isTarget: B => Boolean, neighbors: B => List[(B, Int)], heuristic: B => Int, findAll: Boolean = false): Option[(Int, Map[B, Set[B]], B)] = {
    val distance = scala.collection.mutable.HashMap[B, Int]().withDefault(_ => Int.MaxValue)
    val prev = scala.collection.mutable.HashMap[B, Set[B]]()
    val visited = scala.collection.mutable.HashSet[B]()
    val q = scala.collection.mutable.PriorityQueue[B]()(Ordering.by[B, Int](c => distance(c) + heuristic(c)).reverse)
    val lower: (Int, Int) => Boolean = if findAll then _ <= _ else _ < _

    distance(start) = 0
    q.enqueue(start)

    while (q.nonEmpty) {
      val node = q.dequeue()
      if (isTarget(node)) {
        val d = distance(node)
        return Some((d, prev.toMap, node))
      }
      for (neigh <- neighbors(node)) {
        if (!visited.contains(neigh._1)) {
          val newDistance = distance(node) + neigh._2
          //If "lower" is a strict inequality, the Set[B] of previous entries will always be of size 1 (at most) --> useful if we only care about a single shortest path
          //Otherwise all predecessors leading to a node in the same distance will be stored --> can be used to find all shortest paths
          if (lower(newDistance, distance(neigh._1))) {
            if(newDistance < distance(neigh._1)) {
              prev.update(neigh._1, Set(node))
            } else {
              prev.update(neigh._1, prev(neigh._1) + node)
            }
            distance(neigh._1) = newDistance
            q.enqueue(neigh._1)
          }
        }
      }
      visited.add(node)
    }
    None
  }

  def astar[B](start: B, isTarget: B => Boolean, neighbors: B => List[(B, Int)], heuristic: B => Int): AStarResult[B] =
    _astar(start, isTarget, neighbors, heuristic) match
      case Some(dist, prev, node) => AStarResult(dist, getAnyPath(prev, node))
      case None => AStarResult(Integer.MAX_VALUE, List())

  def astar[B](start: B, target: B, neighbors: B => List[(B, Int)], heuristic: B => Int): AStarResult[B] =
    astar(start, (node: B) => node == target, neighbors, heuristic)

  def dijkstra[B](start: B, target: B, neighbors: B => List[(B, Int)]): AStarResult[B]  =
    astar(start, target, neighbors, _ => 0)

  def dijkstra[B](start: B, isTarget: B => Boolean, neighbors: B => List[(B, Int)]): AStarResult[B]  =
    astar(start, isTarget, neighbors, _ => 0)

  def bfs[A](start: A, target: A, neighbors: A => List[A]): AStarResult[A] =
    bfs(start, (node: A) => node == target, neighbors)

  def bfs[A](start: A, isTarget: A => Boolean, neighbors: A => List[A]): AStarResult[A] =
    dijkstra(start, isTarget, (node: A) => neighbors(node).map(e => (e, 1)))

  def astarAll[B](start: B, isTarget: B => Boolean, neighbors: B => List[(B, Int)], heuristic: B => Int): List[AStarResult[B]] =
    _astar(start, isTarget, neighbors, heuristic, true) match
      case Some(dist, prev, node) =>
        allPaths(node, prev).map(p => AStarResult(dist, p))
      case None =>
        List.empty

  def dijkstraAll[B](start: B, isTarget: B => Boolean, neighbors: B => List[(B, Int)]): List[AStarResult[B]]  =
    astarAll(start, isTarget, neighbors, _ => 0)

  def allPaths[B](node: B, prev: Map[B, Set[B]]): List[List[B]] = prev.get(node) match
    case Some(nodes) =>
      nodes.toList.flatMap(n => allPaths(n, prev)).map(p => p.prepended(node))
    case None =>
      List(List(node))

  //https://stackoverflow.com/a/36960228
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  val vonNeumannNeighborhood: Seq[Pos] = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
  val mooreNeighborhood: Seq[Pos] = vonNeumannNeighborhood ++ Seq((-1, -1), (-1, 1), (1, -1), (1, 1))

  def manhattan(p: Pos, q: Pos): Int = {
    Math.abs(p._1 - q._1) + Math.abs(p._2 - q._2)
  }

  def manhattan(x: Int, y: Int, x1: Int, y1: Int): Int =
    manhattan((x, y), (x1, y1))

  def ints(x: String): Seq[Int] = raw"-?\d+".r.findAllIn(x).map(_.toInt).toSeq

  def longs(x: String): Seq[Long] = raw"-?\d+".r.findAllIn(x).map(_.toLong).toSeq
  
  type Vel = (Int, Int)

  type Pos = (Int, Int)

  type Grid = Map[Pos, Char]

  extension (c: Pos)
    def *(s: Int): Pos = (c.x * s, c.y * s)
    def -(o: Pos): Pos = (c.x - o.x, c.y - o.y)
    def +(o: Pos): Pos = (c.x + o.x, c.y + o.y)
    def %(o: (Int, Int)): Pos = (Math.floorMod(c.x, o._1), Math.floorMod(c.y, o._2))
    def +(d: Dir): Pos = move(d, 1)
    def -(d: Dir): Pos = move(d.reverse, 1)
    def move(d: Dir, step: Int): Pos = (c.x + d.x * step, c.y + d.y * step)
    def x: Int = c._1
    def y: Int = c._2

  final case class Dir private(x: Int, y: Int):
    def clockwise: Dir = Dir(-y, x)
    def counterclockwise: Dir = Dir(y, -x)
    def reverse: Dir = Dir(-x, -y)

  object Dir:
    final val UP: Dir = Dir(0, -1)
    final val DOWN: Dir = Dir(0, 1)
    final val LEFT: Dir = Dir(-1, 0)
    final val RIGHT: Dir = Dir(1, 0)
}