package day8

import Day8._

class Day8Suite extends munit.FunSuite:

  val input = """RL
                |
                |AAA = (BBB, CCC)
                |BBB = (DDD, EEE)
                |CCC = (ZZZ, GGG)
                |DDD = (DDD, DDD)
                |EEE = (EEE, EEE)
                |GGG = (GGG, GGG)
                |ZZZ = (ZZZ, ZZZ)""".stripMargin

  test("Day8 part1") {
    assertEquals(part1(input), 1)
  }

  test("Day8 part2") {
    assertEquals(part2(input), 1)
  }

