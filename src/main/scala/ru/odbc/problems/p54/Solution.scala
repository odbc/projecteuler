package ru.odbc.problems.p54

import scala.io.Source

object Solution extends App {

  final case class Card(value: Int, suit: Char)

  object Card {
    def apply(str: String): Card = {
      val value =
        if (str(0) == 'T') 10
        else if (str(0) == 'J') 11
        else if (str(0) == 'Q') 12
        else if (str(0) == 'K') 13
        else if (str(0) == 'A') 14
        else str(0).asDigit

      Card(value, str(1))
    }
  }

  final case class Hand(cards: List[Card]) {
    def |>(other: Hand): Boolean =
      if (this.ranked.rank > other.ranked.rank) true
      else if (this.ranked.rank < other.ranked.rank) false
      else {
        val rank = this.ranked.rank
        if (rank == 10) false
        else if (rank == 9) this.cards.head.value > other.cards.head.value
        else if (rank == 8) {
          if (this.ranked.rankedCards.head.value > other.ranked.rankedCards.head.value) true
          else if (this.ranked.rankedCards.head.value < other.ranked.rankedCards.head.value) false
          else this.ranked.rest.head.value > other.ranked.rest.head.value
        }
        else if (rank == 7) {
          if (this.ranked.rankedCards.head.value > other.ranked.rankedCards.head.value) true
          else if (this.ranked.rankedCards.head.value < other.ranked.rankedCards.head.value) false
          else this.ranked.rankedCards.drop(3).head.value > other.ranked.rankedCards.drop(3).head.value
        }
        else if (rank == 6) {
          val res =
            this.ranked.rankedCards.zip(other.ranked.rankedCards)
              .foldLeft(Option.empty[Boolean]) { (acc, c) =>
                if (acc.isDefined) acc
                else if (c._1.value > c._2.value) Some(true)
                else if (c._1.value < c._2.value) Some(false)
                else None
              }

          if (res.isEmpty) false else res.get
        }
        else if (rank == 5) this.cards.head.value > other.cards.head.value
        else if (rank == 4) {
          if (this.ranked.rankedCards.head.value > other.ranked.rankedCards.head.value) true
          else if (this.ranked.rankedCards.head.value < other.ranked.rankedCards.head.value) false
          else {
            val res =
              this.ranked.rest.zip(other.ranked.rest)
                .foldLeft(Option.empty[Boolean]) { (acc, c) =>
                  if (acc.isDefined) acc
                  else if (c._1.value > c._2.value) Some(true)
                  else if (c._1.value < c._2.value) Some(false)
                  else None
                }

            if (res.isEmpty) false else res.get
          }
        }
        else if (rank == 3) {
          if (this.ranked.rankedCards.head.value > other.ranked.rankedCards.head.value) true
          else if (this.ranked.rankedCards.head.value < other.ranked.rankedCards.head.value) false
          else if (this.ranked.rankedCards.drop(2).head.value > other.ranked.rankedCards.drop(2).head.value) true
          else if (this.ranked.rankedCards.drop(2).head.value < other.ranked.rankedCards.drop(2).head.value) false
          else this.ranked.rest.head.value > other.ranked.rest.head.value
        }
        else {
          if (this.ranked.rankedCards.head.value > other.ranked.rankedCards.head.value) true
          else if (this.ranked.rankedCards.head.value < other.ranked.rankedCards.head.value) false
          else {
            val res =
              this.ranked.rest.zip(other.ranked.rest)
                .foldLeft(Option.empty[Boolean]) { (acc, c) =>
                  if (acc.isDefined) acc
                  else if (c._1.value > c._2.value) Some(true)
                  else if (c._1.value < c._2.value) Some(false)
                  else None
                }

            if (res.isEmpty) false else res.get
          }
        }
      }

    val ranked: RankedHand =
      (for {
        _ <- getRoyalFlush
        _ <- getStraightFlush
        _ <- getFourOfKind
        _ <- getFullHouse
        _ <- getFlush
        _ <- getStraight
        _ <- getThreeOfKind
        _ <- getTwoPairs
        _ <- getOnePair
        _ <- getHighCard
      } yield ()).left.get

    def getRoyalFlush: Either[RankedHand, Unit] =
      if (cards.map(_.value) == List(14, 13, 12, 11, 10) && hasSameSuits(cards))
        Left(RankedHand(10, cards, Nil))
      else Right()

    def getStraightFlush: Either[RankedHand, Unit] =
      if (cards.zip(cards.tail).forall { case (l, r) => l.value - r.value == 1 } && hasSameSuits(cards))
        Left(RankedHand(9, cards, Nil))
      else Right()

    def getFourOfKind: Either[RankedHand, Unit] =
      if (hasSameValues(cards.init)) Left(RankedHand(8, cards.init, cards.last :: Nil))
      else if (hasSameValues(cards.tail)) Left(RankedHand(8, cards.tail, cards.head :: Nil))
      else Right()

    def getFullHouse: Either[RankedHand, Unit] =
      if (hasSameValues(cards.take(3)) && hasSameValues(cards.drop(3))) Left(RankedHand(7, cards, Nil))
      else if (hasSameValues(cards.take(2)) && hasSameValues(cards.drop(2)))
        Left(RankedHand(7, cards.drop(2) ++ cards.take(2), Nil))
      else Right()

    def getFlush: Either[RankedHand, Unit] =
      if (hasSameSuits(cards)) Left(RankedHand(6, cards, Nil))
      else Right()

    def getStraight: Either[RankedHand, Unit] =
      if (cards.zip(cards.tail).forall { case (l, r) => l.value - r.value == 1 })
        Left(RankedHand(5, cards, Nil))
      else Right()

    def getThreeOfKind: Either[RankedHand, Unit] =
      if (hasSameValues(cards.take(3))) Left(RankedHand(4, cards.take(3), cards.drop(3)))
      else if (hasSameValues(cards.slice(1, 4)))
        Left(RankedHand(4, cards.slice(1, 4), cards.head :: cards.last :: Nil))
      else if (hasSameValues(cards.drop(2))) Left(RankedHand(4, cards.drop(2), cards.take(2)))
      else Right()

    def getTwoPairs: Either[RankedHand, Unit] =
      if (hasSameValues(cards.take(2)) && hasSameValues(cards.slice(2, 4)))
        Left(RankedHand(3, cards.take(2) ++ cards.slice(2, 4), cards.last :: Nil))
      else if (hasSameValues(cards.take(2)) && hasSameValues(cards.slice(3, 5)))
        Left(RankedHand(3, cards.take(2) ++ cards.slice(3, 5), cards.tail.tail.head :: Nil))
      else if (hasSameValues(cards.slice(1, 3)) && hasSameValues(cards.drop(3)))
        Left(RankedHand(3, cards.slice(1, 3) ++ cards.drop(3), cards.head :: Nil))
      else Right()

    def getOnePair: Either[RankedHand, Unit] =
      if (hasSameValues(cards.take(2))) Left(RankedHand(2, cards.take(2), cards.drop(2)))
      else if (hasSameValues(cards.slice(1, 3))) Left(RankedHand(2, cards.slice(1, 3), cards.head :: cards.drop(3)))
      else if (hasSameValues(cards.slice(2, 4))) Left(RankedHand(2, cards.slice(2, 4), cards.take(2) ++ List(cards.last)))
      else if (hasSameValues(cards.drop(3))) Left(RankedHand(2, cards.drop(3), cards.take(3)))
      else Right()

    def getHighCard: Either[RankedHand, RankedHand] = {
      val maxIndex = cards.zipWithIndex.maxBy(_._1.value)._2
      Left(RankedHand(1, List(cards.drop(maxIndex).head), cards.take(maxIndex) ++ cards.drop(maxIndex + 1)))
    }

    private def hasSameValues(cs: List[Card]): Boolean = cs.forall(_.value == cs.head.value)
    private def hasSameSuits(cs: List[Card]): Boolean = cs.forall(_.suit == cs.head.suit)
  }

  final case class RankedHand(rank: Int, rankedCards: List[Card], rest: List[Card])

  object Hand {
    def apply(cards: List[Card]): Hand = new Hand(cards.sortBy(-_.value))
  }

  final case class Round(left: Hand, right: Hand) {
    def leftWins: Boolean = left |> right
  }

  val game = Source.fromResource("p054_poker.txt").getLines
    .map(_.split(" ").map(Card(_)).splitAt(5))
    .map { case (l, r) => Round(Hand(l.toList), Hand(r.toList)) }

  println(game.count(_.leftWins))

}
