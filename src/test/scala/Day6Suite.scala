package day6

import Day6._

class Day6Suite extends munit.FunSuite:

  val input = """Time:      7  15   30
                |Distance:  9  40  200""".stripMargin

  test("Day6 part1") {
    assertEquals(part1(input), 288)
  }

  test("Day6 part2") {
    assertEquals(part2(input), 1)
  }

