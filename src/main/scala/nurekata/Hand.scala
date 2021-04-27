package nurekata

import nurekata.List.*
import nurekata.Rank
import nurekata.Rank.*
import nurekata.Hand.*
import nurekata.Option.*



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
  cs.zip(cs.tail)
    .forall((x, y) => x.suit == y.suit)

def straight(cs: Cards) = 
  val rs = distinct(sorted(ranks(cs)))
  rs.zip(rs.drop(4))
    .find((f, l) => f.value == l.value + 4)
    .map((h, _) => Straight(h))
    .orElse(lowStraight(rs))

def lowStraight(rs: List[Rank]): Option[Straight] = 
  rs.headOption
    .filter(_ == Ace)
    .flatMap(a =>   
       rs.reverse match 
         case Two :: Three :: Four :: Five :: _ => Some(Straight(Five))
         case _ => None
    )

def ranks(cs: Cards): List[Rank] = 
  cs match 
    case Nil => Nil
    case x :: xs => x.rank :: ranks(xs)

def ranks2(cs: Cards): List[Rank] = cs.map(_.rank)

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
      if y.value > x.value then y :: merge(left, ys)
                           else x :: merge(xs, right)

def distinct(sorted: List[Rank]): List[Rank] = 
  sorted match 
    case x :: y :: xs if x == y => distinct(y :: xs)
    case x :: xs => x :: distinct(xs)
    case Nil => Nil

object Hand:
  def apply(cs: Cards): Hand = 
    ???