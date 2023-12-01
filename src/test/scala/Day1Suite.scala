package day1

import Day1._

class Day1Suite extends munit.FunSuite:

  val input1 = """1abc2
                 |pqr3stu8vwx
                 |a1b2c3d4e5f
                 |treb7uchet""".stripMargin

  val input2 = """two1nine
                 |eightwothree
                 |abcone2threexyz
                 |xtwone3four
                 |4nineeightseven2
                 |zoneight234
                 |7pqrstsixteen"""

  test("Day1 part1") {
    assertEquals(part1(input1), 142)
  }

  test("Day1 part2") {
    assertEquals(part2(input2), 1)
  }

