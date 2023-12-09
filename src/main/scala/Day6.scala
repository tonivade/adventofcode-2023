package day6

import scala.io.Source

case class Race(time: Int, distance: Int):
  def waysToWin: Seq[Int] = all.filter(_._2 > distance).map(_._1)
  def all: Seq[(Int, Int)] =
    for {
      i <- 0 to time
    } yield (i, calculate(i))
  def calculate(hold: Int): Int = (time - hold) * hold

// https://adventofcode.com/2023/day/6
object Day6:
  val regex = "\\b(\\d+)\\b".r

  def parse(line: String): List[Int] = regex.findAllIn(line).map(_.toInt).toList

  def part1(input: String): Int = 
    val (times, distances) = input.split("\n") match {
      case Array(a, b) => (parse(a), parse(b))
    }

    val races = (0 until times.size).map(i => Race(times(i), distances(i)))

    races.map(_.waysToWin.size).product

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))
  println(Day6.part2(input))

