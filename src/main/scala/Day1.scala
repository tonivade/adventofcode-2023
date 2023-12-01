package day1

import scala.io.Source

object Day1:
  def part1(input: String): Int =
    input.split("\n")
      .map(line => line.filter(_.isDigit))
      .map(line => s"${line.head}${line.last}")
      .map(_.toInt)
      .sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
//  println(Day1.part2(input))

