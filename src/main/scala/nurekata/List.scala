package nurekata

import scala.annotation.tailrec
import nurekata.Option.*
import nurekata.std.*

enum List[+A]: 
   case Nil
   case ::(override val head: A, override val tail: List[A])

   def tail: List[A] = 
      throw new NoSuchElementException("tail of empty list")

   def head: A = 
      throw new NoSuchElementException("head of empty list")

   def headOption: Option[A] =
      this match 
        case Nil => None
        case x :: _  => Some(x)

   def length: Int = 
      foldLeft(0)((l, _) => l + 1)

   @tailrec
   final def foldLeft[B](z: B)(op: (B, A) => B): B = 
      this match 
         case Nil => z
         case x :: xs => xs.foldLeft(op(z, x))(op)

   def foldRight[B](z: B)(op: (A, B) => B): B = 
      reverse.foldLeft(z)((a, b) => op(b, a))

   def reduceLeft[B >: A](z: B)(op: (B, B) => B): B = 
      foldLeft(z)(op)

   def reduce[B >: A](op: (B, B) => B): B = 
      if isEmpty then throw new NoSuchElementException("reduce on empty list")
                 else tail.reduceLeft(head)(op)

   def isEmpty: Boolean = 
      this match 
         case Nil => true
         case _ => false

   def map[B](f: A => B): List[B] = 
      this match 
         case Nil => Nil 
         case x :: xs => f(x) :: xs.map(f)

   def flatMap[B](f: A => List[B]): List[B] = 
      this match
         case Nil => Nil
         case x :: xs => f(x) ::: xs.flatMap(f)      

   def reverse: List[A] =
      foldLeft(List.empty)((xs, x) => x :: xs)

   def ::[B >: A](a: B): List[B] = 
      List.::(a, this)

   def :::[B >: A](prefix: List[B]): List[B] = 
      prefix match 
         case Nil => this
         case x :: xs => x :: xs ::: this

   def zip[B](that: List[B]): List[(A, B)] = 
      (this, that) match 
         case (x :: xs, y :: ys) => (x, y) :: xs.zip(ys)
         case _ => Nil

   def filter(p: A => Boolean): List[A] = 
      this match 
         case Nil => Nil
         case x :: xs =>
            if p(x) then x :: xs.filter(p)
                    else xs.filter(p)

   def find(p: A => Boolean): Option[A] = 
      this match 
         case Nil => None
         case x :: xs => 
            if p(x) then Some(x)
                    else xs.find(p)

   def forall(p: A => Boolean): Boolean = 
      this match 
         case Nil => true
         case x :: xs => p(x) && xs.forall(p)

   def drop(n: Int): List[A] = 
      if n <= 0 || isEmpty then this
                           else tail.drop(n - 1)

   def splitAt(i: Int): (List[A], List[A]) = 
      (i, this) match
         case (0, _) => (Nil, this)
         case (_, Nil) => (Nil, Nil)
         case (i, x :: xs) => 
            val (left, right) = xs.splitAt(i - 1)
            (x :: left, right)

   def groupBy[K](f: A => K): Map[K, List[A]] = 
      foldLeft(Map.empty)((m, x) => 
         val k = f(x)
         val v = x :: m.getOrElse(k, List.empty)
         m + (k, v)
      )

   def sorted[B >: A](using ord: Ordering[B]): List[A] = 
      val m = this.length / 2
      if m == 0 
      then this
      else 
         val (left, right) = this.splitAt(m)
         merge(left.sorted, right.sorted)

   private def merge[B >: A](left: List[A], right: List[A])(using ord: Ordering[B]): List[A] =
      (left, right) match
         case (Nil, right) => right
         case (left, Nil) => left
         case (x :: xs, y :: ys) => 
            if ord.compare(x, y) > 0 then y :: merge(left, ys)
                                     else x :: merge(xs, right)

   @tailrec
   final def mkString(start: String, sep: String, end: String): String = 
      this match 
         case Nil => start + end
         case c :: Nil => start + c + end  
         case c :: cs => cs.mkString(start + c + sep, sep, end)

   override def toString = mkString("List(", ", ", ")")


object List:
   def empty[A]: List[A] = Nil

   def apply[A](xs: A*): List[A] =
      xs.foldRight(empty[A])((x, ls) => x :: ls)

extension [A](xs: Array[A])
   def toList: List[A] = 
      xs.foldRight(List.empty[A])((x, ls) => x :: ls)
