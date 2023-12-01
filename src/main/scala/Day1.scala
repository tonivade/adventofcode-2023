package day1

import scala.io.Source

object Day1:

  val regex = "\\d|one|two|three|four|five|six|seven|eight|nine".r

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
      case ((current, state), next) if ((state + next).endsWith("one")) => (current + "1", "")
      case ((current, state), next) if ((state + next).endsWith("two")) => (current + "2", "")
      case ((current, state), next) if ((state + next).endsWith("three")) => (current + "3", "")
      case ((current, state), next) if ((state + next).endsWith("four")) => (current + "4", "")
      case ((current, state), next) if ((state + next).endsWith("five")) => (current + "5", "")
      case ((current, state), next) if ((state + next).endsWith("six")) => (current + "6", "")
      case ((current, state), next) if ((state + next).endsWith("seven")) => (current + "7", "")
      case ((current, state), next) if ((state + next).endsWith("eight")) => (current + "8", "")
      case ((current, state), next) if ((state + next).endsWith("nine")) => (current + "9", "")
      case ((current, state), next) => (current, state + next)
    }._1

  def parse2(line: String): String =
    regex.findAllIn(line).map {
      case "one" => "1"
      case "two" => "2"
      case "three" => "3"
      case "four" => "4"
      case "five" => "5"
      case "six" => "6"
      case "seven" => "7"
      case "eight" => "8"
      case "nine" => "9"
      case digit => digit
    }.mkString

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
  println(Day1.part2(input))
  println(Day1.part1(input) + Day1.part2(input))

