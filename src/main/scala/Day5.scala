package day5

import scala.io.Source

case class Range(source: Long, destination: Long, size: Long):
  val endSource = source + size
  def apply(i: Long): Option[Long] = 
    if (i >= source && i < endSource) {
      Some(destination + (i - source))
    } else {
      None
    }

case class Ranges(ranges: List[Range]):
  def apply(i: Long): Long =
    ranges.foldLeft(Option.empty[Long]) {
      case (None, range) => range(i)
      case (some, range) => some
    }.getOrElse(i)

// https://adventofcode.com/2023/day/5
object Day5:

  def parse(input: String): List[Range] = 
    input.split("\n")
      .drop(1)
      .map(_.split("\\s"))
      .map {
        case Array(a, b, c) => Range(b.toLong, a.toLong, c.toLong)
      }
      .toList

  def part1(input: String): Long = 
    val lines = input.split("\n\n")

    val (_, seeds) = lines(0).splitAt(lines(0).indexOf(":") + 1)
    val parsedSeeds = seeds.trim().split("\\s").map(_.toLong)
    val ranges = lines.drop(1).map(parse).map(Ranges.apply)

    def get(i: Int)(j: Long) = ranges(i)(j)

    val getAll = get(0) andThen 
      get(1) andThen 
      get(2) andThen 
      get(3) andThen
      get(4) andThen
      get(5) andThen
      get(6)

    parsedSeeds.map(getAll).min

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day5.txt").getLines().mkString("\n")
  println(Day5.part1(input))
  println(Day5.part2(input))

