package nurekata

enum List[+A]: 
  case Nil
  case ::(head: A, tail: List[A])

  def ::[B >: A](a: B): List[B] = 
    List.::(a, this)

  def length: Int = this match
    case Nil => 0
    case c :: cs => 1 + cs.length

  def splitAt(i: Int): (List[A], List[A]) = 
    (i, this) match
      case (0, _) => (Nil, this)
      case (_, Nil) => (Nil, Nil)
      case (i, x :: xs) => 
        val (left, right) = xs.splitAt(i - 1)
        (x :: left, right)