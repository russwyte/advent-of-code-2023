package aoc2023

class Day7 extends AocTest:
  import Day7.*
  def winnings(hands: List[Hand]): Long =
    hands.sorted.zipWithIndex
      .map((h, i) => h.bid * (i + 1))
      .sum

  test("part 1") {
    val exampleHands = example.split("\n").map(Hand.apply(_, false))
    val inputHands   = input.map(Hand.apply(_, false))
    assertEquals(winnings(exampleHands.toList), 6440L)
    assertEquals(winnings(inputHands.toList), 248569531L)
  }
  test("part 2") {
    val exampleHands = example.split("\n").map(Hand.apply(_, true))
    val inputHands   = input.map(Hand.apply(_, true))
    assertEquals(winnings(exampleHands.toList), 5905L)
    assertEquals(winnings(inputHands.toList), 250382098L)
  }
end Day7
object Day7:
  enum Card(val value: Int):
    // Joker is special, it can be used as any other card to produce the stongest possible hand
    case Joker extends Card(1)
    case Two   extends Card(2)
    case Three extends Card(3)
    case Four  extends Card(4)
    case Five  extends Card(5)
    case Six   extends Card(6)
    case Seven extends Card(7)
    case Eight extends Card(8)
    case Nine  extends Card(9)
    case Ten   extends Card(10)
    case Jack  extends Card(11)
    case Queen extends Card(12)
    case King  extends Card(13)
    case Ace   extends Card(14)
  end Card
  object Card:
    def apply(c: Char, withJoker: Boolean): Card = c match
      case '2' => Card.Two
      case '3' => Card.Three
      case '4' => Card.Four
      case '5' => Card.Five
      case '6' => Card.Six
      case '7' => Card.Seven
      case '8' => Card.Eight
      case '9' => Card.Nine
      case 'T' => Card.Ten
      case 'J' => if withJoker then Card.Joker else Card.Jack
      case 'Q' => Card.Queen
      case 'K' => Card.King
      case 'A' => Card.Ace
    end apply
  end Card
  enum HandType(val strength: Int):
    case FiveOfAKind  extends HandType(7)
    case FourOfAKind  extends HandType(6)
    case FullHouse    extends HandType(5)
    case ThreeOfAKind extends HandType(4)
    case TwoPair      extends HandType(3)
    case OnePair      extends HandType(2)
    case HighCard     extends HandType(1)
  case class Hand(cards: List[Card], bid: Int, withJoker: Boolean = false):
    val grouped =
      cards.groupBy(identity).view.mapValues(_.size).toMap
    val jokers    = grouped.getOrElse(Card.Joker, 0)
    val nonJokers = (grouped - Card.Joker).values.toList.sorted

    val handType: HandType =
      (jokers, nonJokers) match
        case (5, _)                => HandType.FiveOfAKind
        case (4, _)                => HandType.FiveOfAKind
        case (3, List(2))          => HandType.FiveOfAKind
        case (3, List(1, 1))       => HandType.FourOfAKind
        case (2, List(3))          => HandType.FiveOfAKind
        case (2, List(1, 2))       => HandType.FourOfAKind
        case (2, List(1, 1, 1))    => HandType.ThreeOfAKind
        case (1, List(4))          => HandType.FiveOfAKind
        case (1, List(1, 3))       => HandType.FourOfAKind
        case (1, List(2, 2))       => HandType.FullHouse
        case (1, List(1, 1, 2))    => HandType.ThreeOfAKind
        case (1, List(1, 1, 1, 1)) => HandType.OnePair
        case (0, List(5))          => HandType.FiveOfAKind
        case (0, List(1, 4))       => HandType.FourOfAKind
        case (0, List(1, 1, 3))    => HandType.ThreeOfAKind
        case (0, List(2, 3))       => HandType.FullHouse
        case (0, List(1, 2, 2))    => HandType.TwoPair
        case (0, List(1, 1, 1, 2)) => HandType.OnePair
        case _                     => HandType.HighCard
    end handType
  end Hand
  object Hand:
    given Ordering[Hand] with
      def compare(x: Hand, y: Hand): Int =
        val typeOrder = x.handType.strength.compareTo(y.handType.strength)
        if typeOrder != 0 then typeOrder
        else
          val cardsOrder = x.cards.zip(y.cards).map((a, b) => a.value.compareTo(b.value))
          cardsOrder.find(_ != 0).getOrElse(0)
    def apply(s: String, bid: Int, withJoker: Boolean): Hand =
      Hand(s.toList.map(Card.apply(_, withJoker)), bid, withJoker)
    def apply(s: String, withJoker: Boolean): Hand = Hand(s.take(5), s.drop(6).toInt, withJoker)
  end Hand

  val example =
    """32T3K 765
    |T55J5 684
    |KK677 28
    |KTJJT 220
    |QQQJA 483""".stripMargin
end Day7
