package day3

import scala.io.Source

case class Position(x: Int, y: Int) {
  def left: Position = Position(x - 1, y)
  def right: Position = Position(x + 1, y)
  def up: Position = Position(x, y - 1)
  def down: Position = Position(x, y + 1)

  def upRight: Position = Position(x + 1, y - 1)
  def upLeft: Position = Position(x - 1, y - 1)
  def downRight: Position = Position(x + 1, y + 1)
  def downLeft: Position = Position(x - 1, y + 1)

  def adjacent: Seq[Position] = List(left, right, up, down, upLeft, upRight, downLeft, downRight)
}

case class PartNumber(pos: List[(Position, Char)]) {
  def value: Int =
    pos.map(_._2).toList.mkString.toInt

  def contains(position: Position): Boolean =
    position.adjacent.find(pos.map(_._1).contains).isDefined
  
  def add(position: Position, value: Char): PartNumber = 
    PartNumber(pos :+ (position, value))
}

// https://adventofcode.com/2023/day/3
object Day3:

  def part1(input: String): Int = 
    val numbers = input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.filter(_._1.isDigit).map {
          case (value, x) => Position(x, y) -> value
        }.foldLeft(List.empty[PartNumber]) {
          case (Nil, (p1, v1)) => List(PartNumber(List((p1, v1))))
          case (head :: tail, (p1, v1)) if head.pos.last._1.x == p1.x - 1 => head.add(p1, v1) :: tail
          case (head :: tail, (p1, v1)) if head.pos.last._1.x != p1.x - 1 => PartNumber(List((p1, v1))) :: head :: tail
          case (status, _) => status
        }
      }
    val symbols = input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.filterNot(_._1.isDigit).filterNot(_._1 == '.').map {
          case (_, x) => Position(x, y)
        }
      }.toList

    numbers.filter(number => symbols.find(number.contains).isDefined)
      .map(_.value)
      .sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day3.txt").getLines().mkString("\n")
  println(Day3.part1(input))
  println(Day3.part2(input))

