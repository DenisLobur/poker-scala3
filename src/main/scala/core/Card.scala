package core

import Rank.*
import Suit.*

enum Rank:
    case Two, Three, Four, Five, Six, Seven,
        Eight, Nine, Ten, Jack, Queen, King, Ace

    def isBroadway: Boolean =
        ordinal >= Ten.ordinal

    override def toString: String =
        this match
            case Ace   => "A"
            case King  => "K"
            case Queen => "Q"
            case Jack  => "J"
            case Ten   => "T"
            case n     => (n.ordinal + 2).toString()

enum Suit:
    case Hearts, Diamonds, Clubs, Spades

    override def toString: String =
        this match
            case Hearts   => "♥"
            case Diamonds => "♦"
            case Clubs    => "♣"
            case Spades   => "♠"

case class Card(rank: Rank, suit: Suit):
    override def toString: String =
        rank.toString + suit.toString
