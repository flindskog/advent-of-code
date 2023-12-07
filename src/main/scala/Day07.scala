import util.Input

import scala.annotation.tailrec

sealed trait Hand extends Ordered[Hand] {
  val cards: String

  def withCards(cards: String): Hand

  val rank = this match {
    case _: HighCard     => 1
    case _: OnePair      => 2
    case _: TwoPairs     => 3
    case _: ThreeOfAKind => 4
    case _: FullHouse    => 5
    case _: FourOfAKind  => 6
    case _: FiveOfAKind  => 7
  }

  override def compare(that: Hand): Int =
    if (rank == that.rank) {
      cards.compareTo(that.cards)
    } else {
      rank.compareTo(that.rank)
    }
}

case class HighCard(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class OnePair(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class TwoPairs(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class ThreeOfAKind(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class FullHouse(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class FourOfAKind(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}
case class FiveOfAKind(cards: String) extends Hand {
  override def withCards(cards: String): Hand = copy(cards = cards)
}

trait Day07 {
  val data = Input.read("input_07.txt")
  val cardsAndBids = data.map { str =>
    val Array(cards, bid) = str.split(" +")
    cards
      .replaceAll("A", "E")
      .replaceAll("K", "D")
      .replaceAll("Q", "C")
      .replaceAll("J", "B")
      .replaceAll("T", "A")
      -> bid.toLong
  }

  def parseHand(cards: String): Hand = {
    val grouped       = cards.groupBy(identity).mapValues(_.length).toList.sortBy(_._2).reverse
    val (card, count) = grouped.head
    count match {
      case 5 => FiveOfAKind(cards)
      case 4 => FourOfAKind(cards)
      case 3 =>
        grouped.tail.headOption match {
          case Some((_, 2)) => FullHouse(cards)
          case _            => ThreeOfAKind(cards)
        }
      case 2 =>
        grouped.tail.headOption match {
          case Some((_, 2)) => TwoPairs(cards)
          case _            => OnePair(cards)
        }
      case 1 => HighCard(cards)
    }
  }
}

object Day07_1 extends App with Day07 {
  val result = cardsAndBids
    .map(x => parseHand(x._1) -> x._2)
    .sortBy(_._1)
    .zipWithIndex
    .map { case ((_, bid), index) => bid * (index + 1) }

  println(result.sum) // 252656917
}

object Day07_2 extends App with Day07 {
  val cardsNoJoker = "23456789ACDE"
  def parseHandWithJoker(cards: String): Hand =
    cardsNoJoker
      .map(c => cards -> parseHand(cards.replace('1', c)))
      .maxBy(_._2)
      ._2
      .withCards(cards)

  val with1asJoker = cardsAndBids.map { case (cards, bid) => cards.replace('B', '1') -> bid }

  val hands = with1asJoker
    .map(x => parseHandWithJoker(x._1) -> x._2)
    .sortBy(_._1)
    .zipWithIndex
  
  val result = hands.map { case ((_, bid), index) => bid * (index + 1) }.sum
  println(result) // 253499763
}
