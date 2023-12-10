package day8

import Day8._

class Day8Suite extends munit.FunSuite:

  val input1 = """RL
                 |
                 |AAA = (BBB, CCC)
                 |BBB = (DDD, EEE)
                 |CCC = (ZZZ, GGG)
                 |DDD = (DDD, DDD)
                 |EEE = (EEE, EEE)
                 |GGG = (GGG, GGG)
                 |ZZZ = (ZZZ, ZZZ)""".stripMargin
  
  val input2 = """LR
                 |
                 |11A = (11B, XXX)
                 |11B = (XXX, 11Z)
                 |11Z = (11B, XXX)
                 |22A = (22B, XXX)
                 |22B = (22C, 22C)
                 |22C = (22Z, 22Z)
                 |22Z = (22B, 22B)
                 |XXX = (XXX, XXX)""".stripMargin

  test("Day8 part1") {
    assertEquals(part1(input1), 2)
  }

  test("Day8 part1 continue") {
    assertEquals(part1("""LLR
                         |
                         |AAA = (BBB, BBB)
                         |BBB = (AAA, ZZZ)
                         |ZZZ = (ZZZ, ZZZ)""".stripMargin), 6)
  }

  test("Day8 part2") {
    assertEquals(part2(input2), BigInt(6))
  }

