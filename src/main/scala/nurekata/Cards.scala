package nurekata

enum Cards:
  case Nil
  case Cons(head: Card, tail: Cards)

  def length: Int = this match
    case Nil => 0
    case Cons(c, cs) => 1 + cs.length
