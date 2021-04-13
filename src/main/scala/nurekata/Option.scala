package nurekata

enum Option[+A]:
  case None
  case Some(a: A)

  def map[B](f: A => B): Option[B] =
    this match
      case None => None
      case Some(a) => Some(f(a))