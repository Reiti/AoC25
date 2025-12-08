import util.Day

import scala.annotation.tailrec

object Day8 extends Day(8):
  override def solve(): Unit =
    val coords = inputLines.map(l => l.split(",").map(_.toInt)).map(l => (l.head, l(1), l(2)))
    val pairs = coords.combinations(2).toList.sortBy(p => euclideanDistance(p.head, p.last)).map(_.toSet)
    val connected = coords.map(c => c -> Set(c)).toMap

    //Part 1
    println(connect(pairs.take(1000), connected).values.toList.sortBy(_.size).reverse.take(3).map(_.size).product)

    val lastPair = fullyConnect(pairs, connected)

    //Part 2
    println(lastPair.head._1.toLong * lastPair.last._1.toLong)

  @tailrec
  def connect(pairs: List[Set[(Int, Int, Int)]], groups: Map[(Int, Int, Int), Set[(Int, Int, Int)]]): Map[(Int, Int, Int), Set[(Int, Int, Int)]] = pairs match
    case x :: xs => connect(xs, union(groups, x.head, x.last))
    case _ => groups

  @tailrec
  def fullyConnect(pairs: List[Set[(Int, Int, Int)]], groups: Map[(Int, Int, Int), Set[(Int, Int, Int)]]): Set[(Int, Int, Int)] =
    val currPair = pairs.head
    val withCurrPairConnected = union(groups, currPair.head, currPair.last)

    if withCurrPairConnected.size == 1 then
      currPair
    else
      fullyConnect(pairs.tail, withCurrPairConnected)

  def euclideanDistance(a: (Int, Int, Int), b: (Int, Int, Int)): Double =
    Math.sqrt(Math.pow(a._1 - b._1, 2) + Math.pow(a._2 - b._2, 2) + Math.pow(a._3 - b._3, 2))

  def find(sets: Map[(Int, Int, Int), Set[(Int, Int, Int)]], coord: (Int, Int, Int)): (Int, Int, Int) =
    if sets.keySet.contains(coord) then
      coord
    else
      sets.find(e => e._2.contains(coord)).get._1

  def union(sets: Map[(Int, Int, Int), Set[(Int, Int, Int)]], a: (Int, Int, Int), b: (Int, Int, Int)): Map[(Int, Int, Int), Set[(Int, Int, Int)]] =
    val ar = find(sets, a)
    val br = find(sets, b)

    val ns = sets(ar).union(sets(br))

    sets.removed(ar).updated(br, ns)