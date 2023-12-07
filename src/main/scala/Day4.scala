package day4

import scala.io.Source

case class Card(number: Int, winningNumbers: Set[Int], numbers: Set[Int]):
  def points: Int = numbers.intersect(winningNumbers).size match {
    case i if i == 0 => 0
    case i => Math.pow(2, i - 1).toInt
  }

// https://adventofcode.com/2023/day/4
object Day4:
  val regex = "\\b(\\d+)\\b".r

  def parse(line: String): Card =
    val (card, (winningNumbers, numbers)) = line.splitAt(line.indexOf(':')) match {
      case (left, right) => (left.filter(_.isDigit).toInt, right.splitAt(right.indexOf('|')))
    }

    Card(card,
      regex.findAllIn(winningNumbers)
        .map(_.toInt).toSet,
      regex.findAllIn(numbers)
        .map(_.toInt).toSet)

  def part1(input: String): Int =
    input.split("\n")
      .map(parse)
      .map(_.points)
      .sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day4.txt").getLines().mkString("\n")
  println(Day4.part1(input))
  println(Day4.part2(input))

