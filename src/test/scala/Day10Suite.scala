package day10

import Day10._

class Day10Suite extends munit.FunSuite:

  val input = """-L|F7
                |7S-7|
                |L|7||
                |-L-J|
                |L|-JF""".stripMargin

  test("Day10 part1") {
    assertEquals(part1(input), 4)
  }

  test("Day10 part2") {
    assertEquals(part2(input), 1)
  }

