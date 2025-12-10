import util.{Day, Util}

import java.awt.Button
import scala.annotation.tailrec

case class Machine(lights: List[Boolean], buttons: List[Set[Int]], joltage: List[Int]);

object Day10 extends Day(10):
  override def solve(): Unit =
//    val inputLines = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
//                       |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
//                       |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.split("\n").map(_.trim).toList

    val machines = inputLines.map(parseMachines)

    //Part 1
    println(machines.map(findAllOn).sum)


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