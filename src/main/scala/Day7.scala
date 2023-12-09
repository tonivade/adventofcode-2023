package day7

import scala.io.Source

enum Card:
  case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two

enum HandType:
  case FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPair, OnePair, HighCard

case class Hand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card):
  def toList: List[Card] = List(c1, c2, c3, c4, c5)
  def handType: HandType = toList.groupBy(identity).mapValues(_.size).values.toList.sorted.reverse match {
      case List(5) => HandType.FiveOfKind
      case List(4, 1) => HandType.FourOfKind
      case List(3, 2) => HandType.FullHouse
      case List(3, 1, 1) => HandType.ThreeOfKind
      case List(2, 2, 1) => HandType.TwoPair
      case List(2, 1, 1, 1) => HandType.OnePair
      case _ => HandType.HighCard
    }

implicit object HandOrdering extends Ordering[Hand]:
  def compare(a: Hand, b: Hand): Int = 
    val x = a.handType.ordinal compareTo b.handType.ordinal
    val cardsA = a.toList
    val cardsB = b.toList
    (0 until 5).map(i => (cardsA(i), cardsB(i))).foldLeft(x) {
      case (0, (c1, c2)) => c1.ordinal compareTo c2.ordinal
      case (x, _) => x
    }

// https://adventofcode.com/2023/day/7
object Day7:
  import Card._

  def toCard(card: Char): Card =
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

  def parse(input: String): Hand = 
    input.map(toCard) match {
      case Seq(c1, c2, c3, c4, c5) => Hand(c1, c2, c3, c4, c5)
    }
    
  def part1(input: String): Int = 
    input.split("\n")
      .map(line => (parse(line.take(5)), line.drop(6).toInt))
      .sorted
      .reverse
      .zipWithIndex
      .map {
        case ((hand, bid), i) => (i + 1) * bid
      }.sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
  println(Day7.part2(input))

