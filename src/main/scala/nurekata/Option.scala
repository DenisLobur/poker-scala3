package nurekata

enum Option[+A]:
   case None
   case Some(a: A)
 
   def isEmpty = 
      this match 
         case None => true 
         case _ => false
   
   def get: A = 
      this match 
         case None => throw new NoSuchElementException("None.get")
         case Some(a) => a
 
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

   def exists(p: A => Boolean): Boolean = 
      !isEmpty && p(this.get)
 
   def orElse[B >: A](alt: => Option[B]): Option[B] = 
      if isEmpty then alt
                 else this
   
   def getOrElse[B >: A](default: => B): B =
      if isEmpty then default 
                 else this.get

object Option:
   def apply[A](a: A): Option[A] = 
      if a == null then None
                   else Some(a)