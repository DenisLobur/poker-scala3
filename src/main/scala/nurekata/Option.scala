package nurekata

enum Option[+A]:
  case None
  case Some(a: A)

