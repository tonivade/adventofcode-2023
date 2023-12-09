package day7

import Day7._

class Day7Suite extends munit.FunSuite:

  val input = """32T3K 765
                |T55J5 684
                |KK677 28
                |KTJJT 220
                |QQQJA 483""".stripMargin

  test("Day7 part1") {
    assertEquals(part1(input), 6440)
  }

  test("Day7 part2") {
    assertEquals(part2(input), 5905)
  }

