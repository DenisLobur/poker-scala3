package nurekata

enum List[+A]: 
  case Nil
  case ::(head: A, override val tail: List[A])

  def tail: List[A] = 
    throw new NoSuchElementException("tail of empty list")

  def length: Int = 
    this match
      case Nil => 0
      case c :: cs => 1 + cs.length

  def ::[B >: A](a: B): List[B] = 
    List.::(a, this)

  def zip[B](that: List[B]): List[(A, B)] = 
    (this, that) match 
      case (x :: xs, y :: ys) => (x, y) :: xs.zip(ys)
      case _ => Nil

  def splitAt(i: Int): (List[A], List[A]) = 
    (i, this) match
      case (0, _) => (Nil, this)
      case (_, Nil) => (Nil, Nil)
      case (i, x :: xs) => 
        val (left, right) = xs.splitAt(i - 1)
        (x :: left, right)

  final def mkString(start: String, sep: String, end: String): String = 
    this match 
      case Nil => start + end
      case c :: Nil => start + c + end  
      case c :: cs => cs.mkString(start + c + sep, sep, end)
  
  override def toString = mkString("List(", ", ", ")")