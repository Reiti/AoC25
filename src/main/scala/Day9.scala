import util.{Day, Util}
import util.Util.{+, -, Pos, x, y}

object Day9 extends Day(9):
  override def solve(): Unit =
    val tiles: List[Pos] = inputLines.map(l => l.split(",")).map(l => (l.head.toInt, l.last.toInt))

    val edges = tiles.sliding(2).map(e => (e.head, e.last)).toList ++ List((tiles.last, tiles.head))

    val allRectangles = tiles.combinations(2).toList

    //Part 1
    println(allRectangles.map(area).max)

    //Part 2
    println(allRectangles.filter(r => isContained(r, edges, tiles)).map(area).max)

  def area(r: List[Pos]): Long = (Math.abs(r.head._1 - r.last._1) + 1L) * (Math.abs(r.head._2 - r.last._2) + 1L)

  def between(x: Int, a: Int, b: Int): Boolean =
    val min = Math.min(a, b)
    val max = Math.max(a, b)

    min <= x && x < max //Turning the left < into a <= did it

  def onEdge(pos: Pos, edge: (Pos, Pos)): Boolean = between(pos.x, edge._1.x, edge._2.x) && between(pos.y, edge._1.y, edge._2.y)

  def onAnyEdge(pos: Pos, edges: List[(Pos, Pos)]): Boolean = edges.exists(e => onEdge(pos, e))

  def isInside(pos: Pos, edges: List[(Pos, Pos)]): Boolean =
    val left = edges.filter(e => e._1.x <= pos.x && e._2.x <= pos.x).flatMap(e => intersection(((0, pos.y), pos), e)).size % 2
    val right = edges.filter(e => e._1.x >= pos.x && e._2.x >= pos.x).flatMap(e => intersection((pos, (Integer.MAX_VALUE, pos.y)), e)).size % 2

    val up = edges.filter(e => e._1.y <= pos.y && e._2.y <= pos.y).flatMap(e => intersection(((pos.x, 0), pos), e)).size % 2
    val down = edges.filter(e => e._1.y >= pos.y && e._2.y >= pos.y).flatMap(e => intersection((pos, (pos.x, Integer.MAX_VALUE)), e)).size % 2

    onAnyEdge(pos, edges) ||  left == 1 || right == 1 || up == 1 || down == 1

  def allIntersections(l: (Pos, Pos), edges: List[(Pos, Pos)]): List[Pos] = edges.flatMap(e => intersection(l, e))

  def intersection(e1: (Pos, Pos), e2: (Pos, Pos)): Option[Pos] =
    if e1._1.x == e1._2.x then
      if e2._1.x == e2._2.x then
        None
      else if between(e2._2.y, e1._1.y, e1._2.y) && between(e1._1.x, e2._1.x, e2._2.x) then
        Some((e1._1.x, e2._1.y))
      else
        None
    else
      if e2._1.y == e2._2.y then
        None
      else if between(e2._1.x, e1._1.x, e1._2.x) && between(e1._1.y, e2._1.y, e2._2.y) then
        Some(e2._1.x, e1._1.y)
      else
        None

  def isContained(r: List[Pos], edges: List[(Pos, Pos)], tiles: List[Pos]): Boolean =
    val minX = Math.min(r.head.x, r.last.x)
    val minY = Math.min(r.head.y, r.last.y)
    val maxX = Math.max(r.head.x, r.last.x)
    val maxY = Math.max(r.head.y, r.last.y)

    val topLeft = (minX, minY)
    val topRight = (maxX, minY)
    val bottomLeft = (minX, maxY)
    val bottomRight = (maxX, maxY)

    val top = (topLeft, topRight)
    val bottom = (bottomLeft, bottomRight)
    val left = (topLeft, bottomLeft)
    val right = (topRight, bottomRight)

    val topIntersections = allIntersections(top, edges).filter(p => p != topLeft && p != topRight)
    val bottomIntersections = allIntersections(bottom, edges).filter(p => p != bottomLeft && p != bottomRight)
    val leftIntersections = allIntersections(left, edges).filter(p => p != topLeft && p != bottomLeft)
    val rightIntersections = allIntersections(right, edges).filter(p => p != topRight && p != bottomRight)

    val insideTiles = tiles.filter(t => minX < t.x && t.x < maxX && minY < t.y && t.y < maxY)

    isInside(topLeft, edges)
    && isInside(topRight, edges)
    && isInside(bottomLeft, edges)
    && isInside(bottomRight, edges)
    && isInside(topLeft + (1, 1), edges) && isInside(topLeft + (0, 1), edges) && isInside(topLeft + (1, 0), edges)
    && isInside(bottomRight - (1, 1), edges) && isInside(bottomRight - (0, 1), edges) && isInside(bottomRight - (1, 0), edges)
    && isInside(topRight + (-1, 1), edges) && isInside(topRight + (0, 1), edges) && isInside(topRight + (-1, 0), edges)
    && isInside(bottomLeft + (1, -1), edges) && isInside(bottomLeft + (0, -1), edges) && isInside(bottomLeft + (1, 0), edges)
    && topIntersections.forall(p => isInside(p + (1, 0), edges) && isInside(p - (1, 0), edges))
    && bottomIntersections.forall(p => isInside(p + (1, 0), edges) && isInside(p - (1, 0), edges))
    && leftIntersections.forall(p => isInside(p + (0, 1), edges) && isInside(p - (0, 1), edges))
    && rightIntersections.forall(p => isInside(p + (0, 1), edges) && isInside(p - (0, 1), edges))
    && insideTiles.forall(t => Util.mooreNeighborhood.forall(n => isInside(t + n, edges)))