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
  def lastX: Int = pos.last._1.x

  def value: Int =
    pos.map(_._2).toList.mkString.toInt

  def contains(position: Position): Boolean =
    position.adjacent.find(pos.map(_._1).contains).isDefined
  
  def add(position: Position, value: Char): PartNumber = 
    PartNumber(pos :+ (position, value))
}

// https://adventofcode.com/2023/day/3
object Day3:

  def parsePartNumbers(input: String): List[PartNumber] = 
    input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.filter(_._1.isDigit).foldLeft(List.empty[PartNumber]) {
          case (Nil, (value, x)) => List(PartNumber(List((Position(x, y), value))))
          case (head :: tail, (value, x)) if head.lastX == x - 1 => head.add(Position(x, y), value) :: tail
          case (head :: tail, (value, x)) if head.lastX != x - 1 => PartNumber(List((Position(x, y), value))) :: head :: tail
          case (status, _) => status
        }
      }.toList
  
  def parseSymbols(input: String): List[Position] =
    input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.filterNot(_._1.isDigit).filterNot(_._1 == '.').map {
          case (_, x) => Position(x, y)
        }
      }.toList
  
  def parseGears(input: String): List[Position] =
    input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.filter(_._1 == '*').map {
          case (_, x) => Position(x, y)
        }
      }.toList

  def part1(input: String): Int = 
    val numbers = parsePartNumbers(input)
    val symbols = parseSymbols(input)
    numbers.filter(number => symbols.find(number.contains).isDefined)
      .map(_.value)
      .sum

  def part2(input: String): Int = 
    val numbers = parsePartNumbers(input)
    val gears = parseGears(input)
    gears
      .map(symbol => numbers.filter(_.contains(symbol)).map(_.value))
      .filter(_.size > 1)
      .map(_.product)
      .sum

@main def main: Unit =
  val input = Source.fromFile("input/day3.txt").getLines().mkString("\n")
  println(Day3.part1(input))
  println(Day3.part2(input))

