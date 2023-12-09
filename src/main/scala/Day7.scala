package day7

import scala.io.Source

enum Card1:
  case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two

enum Card2:
  case Ace, King, Queen, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Jack

enum HandType:
  case FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPair, OnePair, HighCard

case class Hand1(c1: Card1, c2: Card1, c3: Card1, c4: Card1, c5: Card1):
  def toList: List[Card1] = List(c1, c2, c3, c4, c5)
  def handType: HandType = toList.groupBy(identity).mapValues(_.size).values.toList.sorted.reverse match {
      case List(5) => HandType.FiveOfKind
      case List(4, 1) => HandType.FourOfKind
      case List(3, 2) => HandType.FullHouse
      case List(3, 1, 1) => HandType.ThreeOfKind
      case List(2, 2, 1) => HandType.TwoPair
      case List(2, 1, 1, 1) => HandType.OnePair
      case _ => HandType.HighCard
    }

case class Hand2(c1: Card2, c2: Card2, c3: Card2, c4: Card2, c5: Card2):
  def toList: List[Card2] = List(c1, c2, c3, c4, c5)
  def handType: HandType = 
    val jacks = toList.count(_ == Card2.Jack)
    val groups = toList.filter(_ != Card2.Jack).groupBy(identity).mapValues(_.size).values.toList.sorted.reverse
    (jacks, groups) match {
      case (0, List(5)) => HandType.FiveOfKind
      case (0, List(4, 1)) => HandType.FourOfKind
      case (0, List(3, 2)) => HandType.FullHouse
      case (0, List(3, 1, 1)) => HandType.ThreeOfKind
      case (0, List(2, 2, 1)) => HandType.TwoPair
      case (0, List(2, 1, 1, 1)) => HandType.OnePair
      case (0, _) => HandType.HighCard
      case (1, List(4)) => HandType.FiveOfKind
      case (1, List(3, 1)) => HandType.FourOfKind
      case (1, List(2, 2)) => HandType.FullHouse
      case (1, List(2, 1, 1)) => HandType.ThreeOfKind
      case (1, _) => HandType.OnePair
      case (2, List(3)) => HandType.FiveOfKind
      case (2, List(2, 1)) => HandType.FourOfKind
      case (2, _) => HandType.ThreeOfKind
      case (3, List(2)) => HandType.FiveOfKind
      case (3, _) => HandType.FourOfKind
      case (4, _) => HandType.FiveOfKind
      case _ => HandType.FiveOfKind
    }

implicit object Hand1Ordering extends Ordering[Hand1]:
  def compare(a: Hand1, b: Hand1): Int = 
    val x = a.handType.ordinal compareTo b.handType.ordinal
    val cardsA = a.toList
    val cardsB = b.toList
    (0 until 5).map(i => (cardsA(i), cardsB(i))).foldLeft(x) {
      case (0, (c1, c2)) => c1.ordinal compareTo c2.ordinal
      case (x, _) => x
    }

implicit object Hand2Ordering extends Ordering[Hand2]:
  def compare(a: Hand2, b: Hand2): Int = 
    val x = a.handType.ordinal compareTo b.handType.ordinal
    val cardsA = a.toList
    val cardsB = b.toList
    (0 until 5).map(i => (cardsA(i), cardsB(i))).foldLeft(x) {
      case (0, (c1, c2)) => c1.ordinal compareTo c2.ordinal
      case (x, _) => x
    }

// https://adventofcode.com/2023/day/7
object Day7:

  def toCard1(card: Char): Card1 =
    import Card1._
    card match {
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Ten
      case '9' => Nine
      case '8' => Eight
      case '7' => Seven
      case '6' => Six
      case '5' => Five
      case '4' => Four
      case '3' => Three
      case '2' => Two
    }

  def toCard2(card: Char): Card2 =
    import Card2._
    card match {
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Ten
      case '9' => Nine
      case '8' => Eight
      case '7' => Seven
      case '6' => Six
      case '5' => Five
      case '4' => Four
      case '3' => Three
      case '2' => Two
    }

  def parse1(input: String): Hand1 = 
    input.map(toCard1) match {
      case Seq(c1, c2, c3, c4, c5) => Hand1(c1, c2, c3, c4, c5)
    }

  def parse2(input: String): Hand2 = 
    input.map(toCard2) match {
      case Seq(c1, c2, c3, c4, c5) => Hand2(c1, c2, c3, c4, c5)
    }
    
  def part1(input: String): Int = 
    input.split("\n")
      .map(line => (parse1(line.take(5)), line.drop(6).toInt))
      .sorted
      .reverse
      .zipWithIndex
      .map {
        case ((hand, bid), i) => (i + 1) * bid
      }.sum

  def part2(input: String): Int = 
    input.split("\n")
      .map(line => (parse2(line.take(5)), line.drop(6).toInt))
      .sorted
      .reverse
      .zipWithIndex
      .map {
        case ((hand, bid), i) => (i + 1) * bid
      }.sum

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
  println(Day7.part2(input))

