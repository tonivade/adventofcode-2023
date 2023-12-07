package day5

import scala.io.Source

case class Range(source: Long, destination: Long, size: Long):
  def toMap: Map[Long, Long] =
    (0.toLong until size).map(i => (i + source) -> (i + destination)).toMap

// https://adventofcode.com/2023/day/5
object Day5:

  def parse(input: String): Map[Long, Long] = 
    input.split("\n")
      .drop(1)
      .map(_.split("\\s"))
      .map {
        case Array(a, b, c) => Range(b.toLong, a.toLong, c.toLong)
      }
      .map(_.toMap)
      .reduce(_ ++ _)

  def part1(input: String): Long = 
    val lines = input.split("\n\n")

    val (_, seeds) = lines(0).splitAt(lines(0).indexOf(":") + 1)
    val parsedSeeds = seeds.trim().split("\\s").map(_.toLong)
    val maps = lines.drop(1).map(parse)

    println("parsed")

    def get(i: Int)(j: Long) = maps(i).getOrElse(j, j)

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

