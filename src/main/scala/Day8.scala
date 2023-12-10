package day8

import scala.io.Source
import scala.annotation.tailrec

enum Step:
  case Left, Right

case class Node(label: String, left: String, right: String)

// https://adventofcode.com/2023/day/8
object Day8:
  val regex1 = "([A-Z]{3}) = \\(([A-Z]{3}), ([A-Z]{3})\\)".r
  val regex2 = "([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)".r

  @tailrec
  def walk1(map: Map[String, Node], steps: List[Step], current: String, count: Int = 0): Int =
    if (current == "ZZZ") count
    else steps match {
        case Step.Left :: tail => 
          walk1(map, tail :+ Step.Left, map(current).left, count + 1)
        case Step.Right :: tail => 
          walk1(map, tail :+ Step.Right, map(current).right, count + 1)
        case Nil => count
      }

  @tailrec
  def walk2(map: Map[String, Node], steps: List[Step], paths: Map[String, String], count: Int = 0): Int =
    if (paths.values.forall(_.endsWith("Z"))) count
    else steps match {
        case Step.Left :: tail => 
          walk2(map, tail :+ Step.Left, paths.mapValues(map(_).left).toMap, count + 1)
        case Step.Right :: tail => 
          walk2(map, tail :+ Step.Right, paths.mapValues(map(_).right).toMap, count + 1)
        case Nil => count
      }

  def part1(input: String): Int = 
    val lines = input.split("\n")
    val steps = lines(0).map {
      case 'L' => Step.Left
      case 'R' => Step.Right
    }.toList
    val map = lines.drop(2).map {
      case regex1(label, left, right) => label -> Node(label, left, right)
    }.toMap

    walk1(map, steps, "AAA")

  def part2(input: String): Int = 
    val lines = input.split("\n")
    val steps = lines(0).map {
      case 'L' => Step.Left
      case 'R' => Step.Right
    }.toList
    val map = lines.drop(2).map {
      case regex2(label, left, right) => label -> Node(label, left, right)
    }.toMap

    val paths = map.keys.filter(_.endsWith("A")).map(s => s -> s).toMap

    walk2(map, steps, paths)

@main def main: Unit =
  val input = Source.fromFile("input/day8.txt").getLines().mkString("\n")
  println(Day8.part1(input))
  println(Day8.part2(input))

