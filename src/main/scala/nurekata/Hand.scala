package nurekata

import nurekata.List.*
import nurekata.Rank
import nurekata.Rank.*
import nurekata.Hand.*
import nurekata.Option.*
import nurekata.std.*

type Cards = List[Card]

enum Hand:
   case Straight(high: Rank)
   case Flush(ranks: List[Rank])
   case StraightFlush(high: Rank)
   case RoyalFlush

given rankOrdAsc: Ordering[Rank] = Rank.ord.reverse

//TODO fix case List(10♠️, Q♠️, 2♥️, K♠️, A♠️, J♠️, 10♥️)
def royalFlush(cs: Cards): Option[RoyalFlush.type] = 
   val fs = filterGte10(cs)
   val fl = flush(fs)
   fl match
      case Some(x) => Some(RoyalFlush)
      case _ => None

def filterGte10(cs: List[Card]): List[Card] = 
   cs.filter(c => c.rank >= Ten)

def filterLte10(cs: List[Card]): List[Card] =
   cs.filter(c => c.rank <= Nine)   

def sameSuit(cs: Cards): Boolean = 
   cs.zip(cs.tail)
      .forall((x, y) => x.suit == y.suit)

def straight(cs: Cards) = 
   val rs = distinct(ranks(cs).sorted)
   rs.zip(rs.drop(4))
      .find((f, l) => f.value == l.value + 4)
      .map((h, _) => Straight(h))
      .orElse(lowStraight(rs))

def straightFlush(cs: Cards): Option[StraightFlush] = ???

def flush(cs: Cards): Option[Flush] = 
   cs.groupBy(_.suit)
      .values
      .find(_.length >= 5)
      .map[Flush](cs => Flush(cs.map(_.rank)
                        .sorted))

def lowStraight(rs: List[Rank]): Option[Straight] = 
   rs.headOption
      .filter(_ == Ace)
      .flatMap(_ =>   
         rs.reverse match 
            case Two :: Three :: Four :: Five :: _ => Some(Straight(Five))
            case _ => None
      )

def ranks(cs: Cards): List[Rank] = 
   cs.map(_.rank)

def distinct(sorted: List[Rank]): List[Rank] = 
   sorted match 
      case x :: y :: xs if x == y => distinct(y :: xs)
      case x :: xs => x :: distinct(xs)
      case Nil => Nil

object Hand:
   def apply(cs: Cards): Hand = 
      ???