package day3

import Day3._

class Day3Suite extends munit.FunSuite:

  val input = """467..114..
                |...*......
                |..35..633.
                |......#...
                |617*......
                |.....+.58.
                |..592.....
                |......755.
                |...$.*....
                |.664.598..""".stripMargin

  test("Day3 part1") {
    assertEquals(part1(input), 4361)
  }

  test("Day3 part2") {
    assertEquals(part2(input), 1)
  }

