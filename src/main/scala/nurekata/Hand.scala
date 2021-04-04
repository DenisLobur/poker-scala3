package nurekata

import nurekata.Cards.*

def hasRoyalFlush(cs: Cards) = 
  val fs = filterGte10(cs)
  fs.length == 5 && sameSuit(fs)

def filterGte10(cs: Cards): Cards = cs match 
  case Nil => Nil
  case Cons(c, cs) => if c.rank.value >= 10 then Cons(c, filterGte10(cs)) else filterGte10(cs)

def sameSuit(cs: Cards): Boolean = cs match 
  case Cons(x, Cons(y, cs)) => x.suit == y.suit && sameSuit(Cons(y, cs))
  case _ => true