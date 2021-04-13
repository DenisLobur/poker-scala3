package nurekata

import nurekata.List.*
import nurekata.Rank
import nurekata.Rank.*
import nurekata.Hand.*

type Cards = List[Card]

enum Hand:
  case RoyalFlush
  case Straight(high: Rank)

//TODO fix case List(10♠️, Q♠️, 2♥️, K♠️, A♠️, J♠️, 10♥️)
def hasRoyalFlush(cs: Cards): Boolean = 
  val fs = filterGte10(cs)
  fs.length == 5 && sameSuit(fs)

def filterGte10(cs: List[Card]): List[Card] = 
  cs.filter(c => c.rank >= Ten)

def sameSuit(cs: Cards): Boolean = 
  cs match 
    case x :: y :: cs => 
      x.suit == y.suit && sameSuit(y :: cs)
    case _ => true

def straight(cs: Cards) = 
  val rs = distinct(sorted(ranks(cs)))
  rs.zip(rs.drop(4))
    .filter((f, l) => f.value == l.value + 4)
    .headOption
    .map((h, _) => Straight(h))
    // .orElse(lowStraight(rs))


def ranks(cs: Cards): List[Rank] = 
  cs match 
    case Nil => Nil
    case x :: xs => x.rank :: ranks(xs)

def sorted(cs: List[Rank]): List[Rank] = 
  val m = cs.length / 2
  if m == 0 
  then cs
  else 
    val (left, right) = cs.splitAt(m)
    merge(sorted(left), sorted(right))

private def merge(left: List[Rank], right: List[Rank]): List[Rank] =
  (left, right) match
    case (Nil, right) => right
    case (left, Nil) => left
    case (x :: xs, y :: ys) => 
      if x.value > y.value then x :: merge(xs, right)
                           else y :: merge(left, ys)

def distinct(sorted: List[Rank]): List[Rank] = 
  sorted match 
    case x :: y :: xs if x == y => distinct(y :: xs)
    case x :: xs => x :: distinct(xs)
    case Nil => Nil