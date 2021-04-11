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

  // 1. Implemet toString for List      
  override def toString: String = mkString("List(", ",", ")")
  
  def mkString(start: String, sep: String, end: String): String = {
    def count(list: List[A]): String = list match {
      case Nil => ""
      case head :: Nil => s"$head"
      case head :: tail => s"$head$sep${count(tail)}"
    }

    s"$start${count(this)}$end"
  }

  // 3. implement general distinct
  def distinct[A](sorted: List[A]): List[A] = {
    sorted match {
      case x :: y :: tail => if (x.equals(y)) distinct(y :: tail) else x :: distinct(y :: tail)
      case _ => sorted
    }
  }