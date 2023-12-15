package day10

import Day10._

class Day10Suite extends munit.FunSuite:

  val input1 = """-L|F7
                 |7S-7|
                 |L|7||
                 |-L-J|
                 |L|-JF""".stripMargin

  val input2 = """...........
                 |.S-------7.
                 |.|F-----7|.
                 |.||.....||.
                 |.||.....||.
                 |.|L-7.F-J|.
                 |.|..|.|..|.
                 |.L--J.L--J.
                 |...........""".stripMargin

  test("Day10 part1") {
    assertEquals(part1(input1), 4)
  }

  test("Day10 part2 input1") {
    assertEquals(part2(input1), 1)
  }

  test("Day10 part2 input2") {
    assertEquals(part2(input2), 4)
  }

