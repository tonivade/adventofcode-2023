package day2

import scala.io.Source

enum Color {
  case Red, Green, Blue
}

case class Subset(cubes: Map[Color, Int])
case class Game(number: Int, subsets: List[Subset])

// https://adventofcode.com/2023/day/2
object Day2:
  def parse(line: String): Game =
    val (head, tail) = line.split(":") match {
      case Array(h, t) => (h, t)
    }

    val (_, number) = head.splitAt(4)

    val cubes = tail.split(";").map(_.split(",").map(_.trim()).map(_.split(" ")).map{
      case Array(h, "red") => Color.Red -> h.toInt
      case Array(h, "blue") => Color.Blue -> h.toInt
      case Array(h, "green") => Color.Green -> h.toInt
    }).map((_.toMap)).map(Subset.apply).toList
  
    Game(number.trim().toInt, cubes)

  def possible(red: Int, green: Int, blue: Int)(game: Game): Boolean =
    game.subsets.filter(s => 
      s.cubes.getOrElse(Color.Red, 0) > red ||
      s.cubes.getOrElse(Color.Blue, 0) > blue ||
      s.cubes.getOrElse(Color.Green, 0) > green
      ).isEmpty
  
  def power(game: Game): Int =
    game.subsets.foldLeft((0, 0, 0)) {
      case ((r, g, b), subset) => (
        Math.max(r, subset.cubes.getOrElse(Color.Red, 0)),
        Math.max(g, subset.cubes.getOrElse(Color.Green, 0)),
        Math.max(b, subset.cubes.getOrElse(Color.Blue, 0))
        )
    }.toList.product

  def part1(input: String): Int = 
    input.split("\n")
      .map(parse)
      .filter(possible(12, 13, 14))
      .map(_.number)
      .sum

  def part2(input: String): Int = 
    input.split("\n")
      .map(parse)
      .map(power)
      .sum

@main def main: Unit =
  val input = Source.fromFile("input/day2.txt").getLines().mkString("\n")
  println(Day2.part1(input))
  println(Day2.part2(input))

