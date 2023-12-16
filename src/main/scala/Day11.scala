package day11

import scala.io.Source

enum Direction:
  case North, South, East, West

enum Item:
  case Empty, Galaxy

case class Position(x: Int, y: Int):
  import Direction._
  def movements: Map[Direction, Position] =
    Map(
      East -> Position(x + 1, y),
      West -> Position(x - 1, y),
      South -> Position(x, y + 1),
      North -> Position(x, y - 1)
    )

case class Matrix(map: Map[Position, Item]):
  val width = map.keySet.map(_.x).max + 1
  val height = map.keySet.map(_.y).max + 1
  def expand: Matrix = 

    val emptyRows = (0 until width).map {
      case x => 
        (0 until height).map {
          case y => map(Position(x, y)) 
        }.toList.forall(_ == Item.Empty)
    }
    .zipWithIndex
    .filter {
      case (value, x) => value
    }
    .map(_._2)
    .toList

    val emptyCols = (0 until height).map {
      case y => 
        (0 until width).map {
          case x => map(Position(x, y)) 
        }.toList.forall(_ == Item.Empty)
    }
    .zipWithIndex
    .filter {
      case (value, y) => value
    }
    .map(_._2)
    .toList

    ???

// https://adventofcode.com/2023/day/11
object Day11:
  import Item._
  def part1(input: String): Int = 
    val map = input.split("\n")
      .zipWithIndex 
      .flatMap {
        case (line, y) => line.zipWithIndex.map {
          case ('#', x) => Position(x, y) -> Galaxy
          case (_, x) => Position(x, y) -> Empty
        }
      }.toMap

    val matrix = Matrix(map).expand
    
    ???

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day11.txt").getLines().mkString("\n")
  println(Day11.part1(input))
  println(Day11.part2(input))

