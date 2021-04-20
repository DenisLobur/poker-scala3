package nurekata

enum Option[+A]:
  case None
  case Some(a: A)

  def isEmpty = 
    this match 
      case None => true 
      case _ => false

  def map[B](f: A => B): Option[B] =
    this match
      case None => None
      case Some(a) => Some(f(a))

  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match 
      case None => None
      case Some(a) => f(a)

  def filter(p: A => Boolean): Option[A] = 
    this match 
      case Some(x) if p(x) => this
      case _ => None

  def orElse[B >: A](alt: => Option[B]): Option[B] = 
    if isEmpty then alt
               else this