import util.{Day, Util}
import util.Util.Dir.{DOWN, LEFT, RIGHT}
import util.Util.{Pos, move}


object Day7 extends Day(7):
  override def solve(): Unit =
    val start = inputMap.find(_._2 == 'S').get._1

    //Part 1
    println(findSplits(start, inputMap, Set()).size)

    //Part 2
    println(countQuantumSplits(start, inputMap))

  def findSplits(pos: Pos, map: Map[Pos, Char], visited: Set[Pos]): Set[Pos] =
    if visited.contains(pos) then
      visited
    else
      map.get(pos) match
        case Some('^') => findSplits(pos.move(RIGHT, 1), map, visited.union(findSplits(pos.move(LEFT, 1), map, visited + pos)))
        case Some(_) => findSplits(pos.move(DOWN, 1), map, visited)
        case _ => visited

  def countQuantumSplits(pos: Pos, map: Map[Pos, Char]): Long =
    lazy val countQ: Pos => Long = Util.memoize:
      case p if map.get(p).contains('^') => countQ(p.move(LEFT, 1)) + countQ(p.move(RIGHT, 1))
      case p if map.contains(p) => countQ(p.move(DOWN, 1))
      case _ => 1

    countQ(pos)