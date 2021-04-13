package nurekata

import nurekata.List.*
import nurekata.Rank
import nurekata.Rank.*

type Cards = List[Card]

def hasRoyalFlush(cs: Cards): Boolean = 
  val fs = filterGte10(cs)
  fs.length == 5 && sameSuit(fs)

def filterGte10(cs: List[Card]): List[Card] = 
  cs.filter(c => c.rank >= Ten)

def filterStraight(rs: List[(Rank, Rank)]): List[(Rank, Rank)] = 
  rs.filter((f, l) => f.value == l.value + 4)

def sameSuit(cs: Cards): Boolean = 
  cs match 
    case x :: y :: cs => 
      x.suit == y.suit && sameSuit(y :: cs)
    case _ => true

def straight(cs: Cards) = 
  val rs = distinct(sorted(ranks(cs)))
  filterStraight(rs.zip(rs.drop(4)))
    .headOption

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