package core

import Rank.*
import Suit.*

import scala.annotation.targetName

enum Rank:
    case Two, Three, Four, Five, Six, Seven,
        Eight, Nine, Ten, Jack, Queen, King, Ace

    val value = ordinal + 2

    def isBroadway: Boolean =
        this >= Rank.Ten

    @targetName("greaterThen")
    def >=(other: Rank): Boolean =
        ordinal >= other.ordinal

    override def toString: String =
        this match
            case Ace   => "A"
            case King  => "K"
            case Queen => "Q"
            case Jack  => "J"
            case Ten   => "T"
            case _     => value.toString

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

    @targetName("greaterThen")
    def >=(other: Card): Boolean =
        rank.value >= other.rank.value
