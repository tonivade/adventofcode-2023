package day6

import scala.io.Source

case class Race(time: Long, distance: Long):
  def waysToWin: Seq[Long] = all.filter(_._2 > distance).map(_._1)
  def all: Seq[(Long, Long)] =
    for {
      i <- 0L to time
    } yield (i, calculate(i))
  def calculate(hold: Long): Long = (time - hold) * hold

// https://adventofcode.com/2023/day/6
object Day6:
  val regex = "\\b(\\d+)\\b".r

  def parse1(line: String): List[Int] = regex.findAllIn(line).map(_.toInt).toList
  def parse2(line: String): Long = line.filter(_.isDigit).toLong

  def part1(input: String): Long = 
    val (times, distances) = input.split("\n") match {
      case Array(a, b) => (parse1(a), parse1(b))
    }

    val races = (0 until times.size).map(i => Race(times(i), distances(i)))

    races.map(_.waysToWin.size).product

  def part2(input: String): Long = 
    val race = input.split("\n") match {
      case Array(a, b) => Race(parse2(a), parse2(b))
    }
    race.waysToWin.size

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))
  println(Day6.part2(input))

