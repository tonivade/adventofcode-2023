package day1

import scala.io.Source

object Day1:
  def part1(input: String): Int =
    input.split("\n")
      .map(line => line.filter(_.isDigit))
      .map(line => s"${line.head}${line.last}")
      .map(_.toInt)
      .sum

  def part2(input: String): Int =
    input.split("\n")
      .map(line => replaceAll(line))
      .map(line => {
        println(line)
        line
      })
      .map(line => line.filter(_.isDigit))
      .map(line => s"${line.head}${line.last}")
      .map(_.toInt)
      .sum

  def replaceAll(line: String): String =
    val result = line.foldLeft(("", "")) {
      case ((current, state), next) if ((state + next).endsWith("one")) => (current + state.dropRight(2) + "1", "")
      case ((current, state), next) if ((state + next).endsWith("two")) => (current + state.dropRight(2) + "2", "")
      case ((current, state), next) if ((state + next).endsWith("three")) => (current + state.dropRight(4) + "3", "")
      case ((current, state), next) if ((state + next).endsWith("four")) => (current + state.dropRight(3) + "4", "")
      case ((current, state), next) if ((state + next).endsWith("five")) => (current + state.dropRight(3) + "5", "")
      case ((current, state), next) if ((state + next).endsWith("six")) => (current + state.dropRight(2) + "6", "")
      case ((current, state), next) if ((state + next).endsWith("seven")) => (current + state.dropRight(4) + "7", "")
      case ((current, state), next) if ((state + next).endsWith("eight")) => (current + state.dropRight(4) + "8", "")
      case ((current, state), next) if ((state + next).endsWith("nine")) => (current + state.dropRight(3) + "9", "")
      case ((current, state), next) if (next.isDigit) => (current + state + next, "")
      case ((current, state), next) => (current, state + next)
    }
    result._1 + result._2

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
  println(Day1.part2(input))

