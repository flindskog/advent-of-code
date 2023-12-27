package aoc.y2023

sealed trait Hand extends Ordered[Hand]:
  val cards: String

  def withCards(cards: String): Hand

  val rank = this match
    case _: HighCard     => 1
    case _: OnePair      => 2
    case _: TwoPairs     => 3
    case _: ThreeOfAKind => 4
    case _: FullHouse    => 5
    case _: FourOfAKind  => 6
    case _: FiveOfAKind  => 7

  override def compare(that: Hand): Int =
    if rank == that.rank then cards.compareTo(that.cards)
    else rank.compareTo(that.rank)

end Hand

case class HighCard(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class OnePair(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class TwoPairs(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class ThreeOfAKind(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class FullHouse(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class FourOfAKind(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

case class FiveOfAKind(cards: String) extends Hand:
  override def withCards(cards: String): Hand = copy(cards = cards)

object Day07 extends Aoc2023("input_07.txt"):
  val cardsAndBids = input.map { str =>
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
    val grouped    = cards.groupBy(identity).mapValues(_.length).toList.sortBy(_._2).reverse
    val (_, count) = grouped.head
    count match
      case 5 => FiveOfAKind(cards)
      case 4 => FourOfAKind(cards)
      case 3 =>
        grouped.tail.headOption match
          case Some((_, 2)) => FullHouse(cards)
          case _            => ThreeOfAKind(cards)
      case 2 =>
        grouped.tail.headOption match
          case Some((_, 2)) => TwoPairs(cards)
          case _            => OnePair(cards)
      case 1 => HighCard(cards)
  }

  val result = cardsAndBids
    .map(x => parseHand(x._1) -> x._2)
    .sortBy(_._1)
    .zipWithIndex
    .map { case ((_, bid), index) => bid * (index + 1) }

  println(result.sum) // 252656917

  val cardsNoJoker = "23456789ACDE"
  def parseHandWithJoker(cards: String): Hand =
    cardsNoJoker
      .map(c => cards -> parseHand(cards.replace('1', c)))
      .maxBy(_._2)
      ._2
      .withCards(cards)

  val with1asJoker = cardsAndBids.map((cards, bid) => cards.replace('B', '1') -> bid)

  val hands = with1asJoker
    .map(x => parseHandWithJoker(x._1) -> x._2)
    .sortBy(_._1)
    .zipWithIndex

  val result2 = hands.map { case ((_, bid), index) => bid * (index + 1) }.sum
  println(result2) // 253499763
