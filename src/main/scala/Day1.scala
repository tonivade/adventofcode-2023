package day1

import scala.io.Source

// https://adventofcode.com/2023/day/1
object Day1:

  def part1(input: String): Int =
    input.split("\n")
      .map(line => line.filter(_.isDigit))
      .map(line => s"${line.head}${line.last}")
      .map(_.toInt)
      .sum

  def part2(input: String): Int =
    input.split("\n")
      .map(parse)
      .map(line => s"${line.head}${line.last}")
      .map(_.toInt)
      .sum

  def parse(line: String): String =
    line.foldLeft(("", "")) {
      case ((current, state), next) if (next.isDigit) => (current + next, "")
      case ((current, state), next) if ((state + next).endsWith("one")) => (current + "1", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("two")) => (current + "2", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("three")) => (current + "3", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("four")) => (current + "4", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("five")) => (current + "5", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("six")) => (current + "6", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("seven")) => (current + "7", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("eight")) => (current + "8", (state + next).takeRight(4))
      case ((current, state), next) if ((state + next).endsWith("nine")) => (current + "9", (state + next).takeRight(4))
      case ((current, state), next) => (current, state + next)
    }._1

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
  println(Day1.part2(input))

