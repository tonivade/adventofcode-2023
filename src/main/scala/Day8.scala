package day8

import scala.io.Source
import scala.annotation.tailrec

enum Step:
  case Left, Right

case class Node(label: String, left: String, right: String)

// https://adventofcode.com/2023/day/8
object Day8:
  val regex = "([A-Z]{3}) = \\(([A-Z]{3}), ([A-Z]{3})\\)".r

  @tailrec
  def walk(map: Map[String, Node], steps: List[Step], current: String, count: Int = 0): Int =
    if (current == "ZZZ") count
    else steps match {
        case Step.Left :: tail => walk(map, tail :+ Step.Left, map(current).left, count + 1)
        case Step.Right :: tail => walk(map, tail :+ Step.Right, map(current).right, count + 1)
        case Nil => count
      }

  def part1(input: String): Int = 
    val lines = input.split("\n")
    val steps = lines(0).map {
      case 'L' => Step.Left
      case 'R' => Step.Right
    }.toList
    val map = lines.drop(2).map {
      case regex(label, left, right) => label -> Node(label, left, right)
    }.toMap

    walk(map, steps, "AAA")

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day8.txt").getLines().mkString("\n")
  println(Day8.part1(input))
  println(Day8.part2(input))

