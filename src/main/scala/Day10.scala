import util.{Day, Util}

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

case class Machine(lights: List[Boolean], buttons: List[Set[Int]], joltage: List[Int]);

object Day10 extends Day(10):
  override def solve(): Unit =
    val machines = inputLines.map(parseMachines)

    //Part 1
    println(machines.map(findAllOn).sum)

    //Part 2
    println(machines.map(findJoltage).sum)

  def parseMachines(line: String): Machine =
    val split = line.split(" ")
    val lights = split.head.drop(1).init.map(c => if c == '#' then true else false).toList
    val buttons = split.drop(1).init.map(b => b.drop(1).init.split(",").map(_.toInt).toSet).toList
    val joltage = split.last.drop(1).init.split(",").map(_.toInt).toList

    Machine(lights, buttons, joltage)

  def findAllOn(machine: Machine): Int =
    @tailrec
    def _findAllOn(states: Set[List[Boolean]], depth: Int): Int =
      if states.contains(machine.lights) then
        depth
      else
        _findAllOn(states.flatMap(l => toggleAll(l, machine.buttons)), depth + 1)

    _findAllOn(Set(List.fill(machine.lights.length)(false)), 0)

  def toggleAll(lights: List[Boolean], buttons: List[Set[Int]]): Set[List[Boolean]] =
    buttons.map(b => toggle(lights, b)).toSet

  def toggle(lights: List[Boolean], button: Set[Int]): List[Boolean] =
    lights.zipWithIndex.map(l => if button.contains(l._2) then !l._1 else l._1)

  def findLowestToMax(buttons: List[Set[Int]], joltage: List[Int]): (Int, List[Set[Int]]) =
    val presses = buttons.map(b => findMax(b, joltage)).min

    (presses, buttons.filter(b => findMax(b, joltage) == presses))

  def findMax(button: Set[Int], joltage: List[Int]): Int =
    LazyList.from(0).takeWhile(f => button.forall(b => f <= joltage(b))).last


  def isValid(current: List[Int], joltage: List[Int]): Boolean =
    current.zip(joltage).forall(z => z._1 <= z._2)

  def findJoltage(machine: Machine): Long =
    val currMin: AtomicInteger = new AtomicInteger(Integer.MAX_VALUE)

    def _find(remainingButtons: List[Set[Int]], currentJoltage: List[Int], presses: Int): Long =
      if presses >= currMin.get() then
        Integer.MAX_VALUE
      else if currentJoltage == machine.joltage then
        currMin.updateAndGet(v => if v > presses then presses else v)
        presses
      else
        val leastButtons = currentJoltage.indices.filter(i => currentJoltage(i) != machine.joltage(i)).minBy(c => remainingButtons.count(b => b.contains(c)))
        val relevantButtons = remainingButtons.filter(b => b.contains(leastButtons))
        val buttonCount = relevantButtons.size
        val missingJoltage = machine.joltage(leastButtons) - currentJoltage(leastButtons)

        if buttonCount == 0 then
          Integer.MAX_VALUE
        else
          val possiblePresses = partitions(buttonCount, missingJoltage)

          possiblePresses.par.map(p => pressButtons(relevantButtons, p, currentJoltage)).filter(j => isValid(j, machine.joltage)).map(j => _find(remainingButtons.filter(b => !b.contains(leastButtons)), j, presses + missingJoltage)).seq.minOption.getOrElse(Integer.MAX_VALUE)

    _find(machine.buttons, List.fill(machine.joltage.length)(0), 0)

  def pressButtons(buttons: List[Set[Int]], presses: List[Int], currentJoltage: List[Int]): List[Int] = currentJoltage.indices.map(i => valueAt(i, buttons, presses)).toList.zip(currentJoltage).map(z => z._1 + z._2)

  def valueAt(i: Int, buttons: List[Set[Int]], presses: List[Int]): Int = presses.zip(buttons).map(z => if z._2.contains(i) then z._1 else 0).sum

  def partitions(count: Int, value: Int): List[List[Int]] =
    if count == 1 then
      List(List(value))
    else
      (0 to value).flatMap(v => partitions(count - 1, value - v).map(p => p.prepended(v))).toList