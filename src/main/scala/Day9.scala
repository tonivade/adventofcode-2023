package day9

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2023/day/9
object Day9:
  val regex = "-?\\d+".r

  @tailrec
  def reduce(input: List[Int], state: List[List[Int]] = List.empty): List[List[Int]] =
    val next = input.sliding(2).map {
      case List(a, b) => b - a
    }.toList
    if (next.forall(_ == 0))
      next :: state
    else 
      reduce(next, next :: state)

  @tailrec
  def inferNext(input: List[List[Int]], next: Int = 0): Int =
    input match {
      case head :: tail => inferNext(tail, head.last + next)
      case Nil => next
    }

  @tailrec
  def inferBack(input: List[List[Int]], next: Int = 0): Int =
    input match {
      case head :: tail => inferBack(tail, head.head - next)
      case Nil => next
    }

  def part1(input: String): Int =
    input.split("\n")
      .map(line => regex.findAllIn(line).map(_.toInt).toList)
      .map(l => reduce(l) :+ l)
      .map(l => inferNext(l))
      .sum

  def part2(input: String): Int = 
    input.split("\n")
      .map(line => regex.findAllIn(line).map(_.toInt).toList)
      .map(l => reduce(l) :+ l)
      .map(l => inferBack(l))
      .sum

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))

