import util.Day

import util.Util
import util.Util.Pos
import util.Util.+

object Day4 extends Day(4):
  override def solve(): Unit =
    //Part 1
    println(inputMap.count(t => t._2 == '@' && isAccessible(t._1, inputMap)))

    val stable = Iterator.iterate(inputMap)(m => m.filterNot(e => e._2 == '@' && isAccessible(e._1, m))).find(m => m.count(e => e._2 == '@' && isAccessible(e._1, m)) == 0).get

    //Part 2
    println(inputMap.count(e => e._2 == '@') - stable.count(e => e._2 == '@'))

  def isAccessible(pos: Pos, map: Map[Pos, Char]): Boolean =
    Util.mooreNeighborhood.map(n => pos + n).flatMap(map.get).count(_ == '@') < 4