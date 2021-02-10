package com.evolutiongaming.adt

import com.evolutiongaming.adt.ADT.Error.InvalidBoardSize
import com.evolutiongaming.adt.ADT.Hand.OrderedHand

import scala.collection.SortedSet

object ADT {

  sealed trait Suit

  object Suit {
    final case object Diamond extends Suit
    final case object Spade extends Suit
    final case object Club extends Suit
    final case object Heart extends Suit
  }

  sealed trait Rank

  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Hand

  object Hand {
    final case class OmahaHand(cards: Set[Card]) extends AnyVal with Hand
    final case class TexasHand(cards: Set[Card]) extends AnyVal with Hand
    final case class OrderedHand(hand: Hand, combination: PokerCombination) extends Hand
  }

  sealed abstract class ErrorMessage(value: String) {
    def message: String = "Error: " + value
  }

  object Error {
    final case object InvalidBoardSize extends ErrorMessage("Invalid board size")
  }

  sealed abstract case class Board private(cards: List[Card])

  object Board {
    def create(cards: List[Card]): Either[ErrorMessage, Board] = {
      cards match {
        case board if board.length != 5 => Left(InvalidBoardSize)
        case _ => Right(new Board(cards) {})
      }
    }
  }

  sealed trait PokerCombination

  object PokerCombination {
    final object StraightFlush extends PokerCombination
    final object FourOfAKind extends PokerCombination
    final object FullHouse extends PokerCombination
    final object Flush extends PokerCombination
    final object Straight extends PokerCombination
    final object ThreeOfAKind extends PokerCombination
    final object TwoPair extends PokerCombination
    final object Pair extends PokerCombination
    final object HighCard extends PokerCombination
  }

  final case class TestCase(board: Board, hands: Set[Hand])

  final case class TestResult(board: Board, hands: SortedSet[OrderedHand])
}
