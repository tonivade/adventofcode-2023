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
                 |7pqrstsixteen""".stripMargin

  test("Day1 part1") {
    assertEquals(part1(input1), 142)
  }

  test("Day1 part2") {
    assertEquals(part2(input2), 281)
  }

  test("Day2 part2 with overlaping") {
    assertEquals(Day1.parse("twone"), "21")
    assertEquals(Day1.parse("eighthree"), "83")
    assertEquals(Day1.parse("sevenine"), "79")
    assertEquals(Day1.parse("nineight"), "98")
    assertEquals(Day1.parse("oneight"), "18")
  }

