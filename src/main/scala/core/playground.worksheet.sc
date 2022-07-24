import core.Rank.*
import core.Suit.*
import core.*
import ListCard.*

import scala.annotation.targetName

enum ListCard:
    case Nil
    @targetName("Cons") case ::(hd: Card, tl: ListCard)

    def head: Card =
        this match
            case Nil    => throw new NoSuchElementException
            case h :: _ => h

    def last: Card =
        this match
            case Nil      => throw new NoSuchElementException
            case h :: Nil => h
            case _ :: tl  => tl.last

    def ::(h: Card): ListCard =
        ListCard.::(h, this)

    def length: Int =
        this match
            case Nil     => 0
            case h :: tl => 1 + tl.length

    def splitAt(i: Int): (ListCard, ListCard) =
        if (i <= 0) then (Nil, this)
        else
            this match
                case Nil => (Nil, Nil)
                case h :: tl =>
                    val (l, r) = tl.splitAt(i - 1)
                    (h :: l, r)

    def sorted: ListCard =
        val m = length / 2
        if m == 0 then this
        else
            val (l, r) = splitAt(m)
            merge(l.sorted, l.sorted)

    def merge(left: ListCard, right: ListCard): ListCard =
        (left, right) match
            case (Nil, _) => right
            case (_, Nil) => left
            case (l :: ls, r :: rs) =>
                if l >= r
                then l :: merge(ls, right)
                else r :: merge(left, rs)

    def forall(p: Card => Boolean): Boolean =
        this match
            case Nil     => true
            case h :: tl => p(h) && tl.forall(p)

    def exists(p: Card => Boolean): Boolean =
        this match
            case Nil     => false
            case h :: tl => p(h) || tl.exists(p)

    def mkString(start: String, sep: String, end: String): String =
        this match
            case Nil      => start + end
            case h :: Nil => start + h + end
            case h :: tl  => tl.mkString(start + h + sep, sep, end)

    override def toString: String =
        mkString("ListCard(", ", ", ")")

def isRoyalFlush(cs: ListCard): Boolean =
    cs.forall(p => p.suit == cs.head.suit && p.rank.isBroadway)

def isFlush(cs: ListCard): Boolean =
    cs.forall(p => p.suit == cs.head.suit)

 def isStraight(cs: ListCard): Boolean =
    val sorted = cs.sorted
    sorted.head.rank.ordinal == sorted.last.rank.ordinal + 4

 def isStraightFlush(cs: ListCard): Boolean =
    isFlush(cs) && isStraight(cs)

val cs: ListCard = Card(Six, Diamonds) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
    Card(Seven, Diamonds) :: Card(Ten, Diamonds) :: Nil
isStraightFlush(cs)

val card = Card(Ten, Diamonds)
card.rank
card.suit

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted



Ten.isBroadway
Jack.isBroadway
Two.isBroadway
