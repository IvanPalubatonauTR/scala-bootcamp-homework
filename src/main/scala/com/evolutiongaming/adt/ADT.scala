package com.evolutiongaming.adt

import com.evolutiongaming.adt.ADT.Error.{InvalidBoardSize, InvalidHandSize}

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

    sealed abstract case class OmahaHand private(cards: Set[Card]) extends Hand

    object OmahaHand {
      def create(cards: Set[Card]): Either[ErrorMessage, Hand] = cards match {
        case omaha if omaha.size != 4 => Left(InvalidHandSize)
        case _ => Right(new OmahaHand(cards) {})
      }
    }
    sealed abstract case class TexasHand private(cards: Set[Card]) extends Hand

    object TexasHand {
      def create(cards: Set[Card]): Either[ErrorMessage, Hand] = cards match {
        case texas if texas.size != 2 => Left(InvalidHandSize)
        case _ => Right(new TexasHand(cards) {})
      }
    }

  }

  final case class OrderedHand(hand: Hand, combination: PokerCombination)

  sealed abstract class ErrorMessage(value: String) {
    def message: String = "Error: " + value
  }

  object Error {
    final case object InvalidBoardSize extends ErrorMessage("Invalid board size")
    final case object InvalidHandSize extends ErrorMessage("Invalid hand size")
  }

  sealed abstract case class Board private(cards: Set[Card])

  object Board {
    def create(cards: Set[Card]): Either[ErrorMessage, Board] = {
      cards match {
        case board if board.size != 5 => Left(InvalidBoardSize)
        case _ => Right(new Board(cards) {})
      }
    }
  }

  sealed trait PokerCombination {
    def weight:Int
  }

  object PokerCombination {
    final object StraightFlush extends PokerCombination {
      override def weight: Int = 9
    }
    final object FourOfAKind extends PokerCombination {
      override def weight: Int = 8
    }
    final object FullHouse extends PokerCombination {
      override def weight: Int = 7
    }
    final object Flush extends PokerCombination {
      override def weight: Int = 6
    }
    final object Straight extends PokerCombination {
      override def weight: Int = 5
    }
    final object ThreeOfAKind extends PokerCombination {
      override def weight: Int = 4
    }
    final object TwoPair extends PokerCombination {
      override def weight: Int = 3
    }
    final object Pair extends PokerCombination {
      override def weight: Int = 2
    }
    final object HighCard extends PokerCombination {
      override def weight: Int = 1
    }
  }

  final case class TestCase(board: Board, hands: Set[Hand])

  final case class TestResult(board: Board, hands: SortedSet[OrderedHand])

}
