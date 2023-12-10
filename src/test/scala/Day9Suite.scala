package day9

import Day9._

class Day9Suite extends munit.FunSuite:

  val input = """0 3 6 9 12 15
                |1 3 6 10 15 21
                |10 13 16 21 30 45""".stripMargin

  test("Day9 part1") {
    assertEquals(part1(input), 114)
  }

  test("Day9 part2") {
    assertEquals(part2(input), 1)
  }

