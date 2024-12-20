package day10

import scala.io.Source
import scala.annotation.tailrec

enum Direction:
  case North, South, East, West

case class Position(x: Int, y: Int):
  import Direction._
  def movements: Map[Direction, Position] =
    Map(
      East -> Position(x + 1, y),
      West -> Position(x - 1, y),
      South -> Position(x, y + 1),
      North -> Position(x, y - 1)
    )

enum Pipe:
  case NorthToSouth,  // |
    EastToWest,       // -
    NorthToEast,      // L
    NorthToWest,      // J
    SouthToEast,      // F
    SouthToWest,      // 7
    Ground,           // .
    Start             // S

case class Matrix(map: Map[Position, Pipe]):

  val start: Position = map.find {
    case (position, pipe) => pipe == Pipe.Start
  }.map(_._1).get

  def isConnected(a: Position, b: Position, direction: Direction): Boolean =
    import Direction._
    import Pipe._
    direction match {
      case North => (map(a), map(b)) match {
        // |
        // | <
        case (NorthToSouth, NorthToSouth) => true
        // 7
        // | <
        case (NorthToSouth, SouthToEast) => true
        // F
        // | <
        case (NorthToSouth, SouthToWest) => true

        // |
        // L <
        case (NorthToEast, NorthToSouth) => true
        // 7
        // L <
        case (NorthToEast, SouthToEast) => true
        // F
        // L <
        case (NorthToEast, SouthToWest) => true

        // |
        // J <
        case (NorthToWest, NorthToSouth) => true
        // 7
        // J <
        case (NorthToWest, SouthToEast) => true
        // F
        // J <
        case (NorthToWest, SouthToWest) => true

        case (_, Start) => true

        case _ => false
      }

      case South => (map(a), map(b)) match {
        // | <
        // |
        case (NorthToSouth, NorthToSouth) => true
        // | <
        // L
        case (NorthToSouth, NorthToEast) => true
        // | <
        // J
        case (NorthToSouth, NorthToWest) => true

        // F <
        // |
        case (SouthToEast, NorthToSouth) => true
        // F <
        // L
        case (SouthToEast, NorthToEast) => true
        // F <
        // J
        case (SouthToEast, NorthToWest) => true

        // 7 <
        // |
        case (SouthToWest, NorthToSouth) => true
        // 7 <
        // L
        case (SouthToWest, NorthToEast) => true
        // 7 <
        // J
        case (SouthToWest, NorthToWest) => true

        case (_, Start) => true

        case _ => false
      }

      case East => (map(a), map(b)) match {
        // v
        // --
        case (EastToWest, EastToWest) => true
        // v
        // -J
        case (EastToWest, NorthToWest) => true
        // v
        // -7
        case (EastToWest, SouthToWest) => true

        // v
        // F-
        case (SouthToEast, EastToWest) => true
        // v
        // FJ
        case (SouthToEast, NorthToWest) => true
        // v
        // F7
        case (SouthToEast, SouthToWest) => true

        // v
        // L-
        case (NorthToEast, EastToWest) => true
        // v
        // LJ
        case (NorthToEast, NorthToWest) => true
        // v
        // L7
        case (NorthToEast, SouthToWest) => true

        case (_, Start) => true

        case _ => false
      }

      case West => (map(a), map(b)) match {
        //  v
        // --
        case (EastToWest, EastToWest) => true
        //  v
        // L-
        case (EastToWest, NorthToEast) => true
        //  v
        // F-
        case (EastToWest, SouthToEast) => true

        //  v
        // -7
        case (SouthToWest, EastToWest) => true
        //  v
        // L7
        case (SouthToWest, NorthToEast) => true
        //  v
        // F7
        case (SouthToWest, SouthToEast) => true

        //  v
        // -J
        case (NorthToWest, EastToWest) => true
        //  v
        // LJ
        case (NorthToWest, NorthToEast) => true
        //  v
        // FJ
        case (NorthToWest, SouthToEast) => true

        case (_, Start) => true

        case _ => false
      }
    }

// https://adventofcode.com/2023/day/10
object Day10:

  def toPipe(char: Char): Pipe =
    import Pipe._
    char match {
      case '|' => NorthToSouth
      case '-' => EastToWest
      case 'L' => NorthToEast
      case 'J' => NorthToWest
      case 'F' => SouthToEast
      case '7' => SouthToWest
      case '.' => Ground
      case 'S' => Start
    }

  @tailrec
  def move(current: Position, matrix: Matrix, state: List[Position] = List.empty): List[Position] =
    if (current == matrix.start) current :: state
    else
      val next = current.movements.filter {
        // filter positions outside the board
        case (_, position) => matrix.map.contains(position)
      }.filterNot {
        // dont go backwards
        case (_, position) => position == state.head
      }.filter {
        // get only valid moves
        case (direction, position) => matrix.isConnected(current, position, direction)
      }
      // it should be just one valid move or none
      next.headOption match {
        case Some(p) => move(p._2, matrix, current :: state)
        case None => Nil
      }

  def part1(input: String): Int =
    val matrix = Matrix(input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.map {
          case (char, x) => Position(x, y) -> toPipe(char)
        }
      }.toMap
    )

    val path = matrix.start.movements
      .mapValues(p => move(p, matrix, List(matrix.start)))
      .filterNot(_._2.isEmpty)
      .head._2

    val result = path.zipWithIndex.drop(1).toSet intersect path.reverse.zipWithIndex.dropRight(1).toSet

    result.head._2

  def part2(input: String): Int = 
    val matrix = Matrix(input.split("\n")
      .zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.map {
          case (char, x) => Position(x, y) -> toPipe(char)
        }
      }.toMap
    )

    val path = matrix.start.movements
      .mapValues(p => move(p, matrix, List(matrix.start)))
      .filterNot(_._2.isEmpty)
      .head._2

    // https://en.wikipedia.org/wiki/Shoelace_formula
    val area = path.sliding(2).map {
      case List(a, b) => (a.x * b.y) - (a.y * b.x) 
    }.sum.abs / 2

    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    area - (path.size / 2) + 1

@main def main: Unit =
  val input = Source.fromFile("input/day10.txt").getLines().mkString("\n")
  println(Day10.part1(input))
  println(Day10.part2(input))

