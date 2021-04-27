package nurekata

import scala.annotation.tailrec
import nurekata.Option.*

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
    this match
      case Nil => 0
      case c :: cs => 1 + cs.length

  def isEmpty: Boolean = 
    this match 
      case Nil => true
      case _ => false

  def reverse: List[A] = 
    def loop(xs: List[A], acc: List[A]): List[A] =
      xs match 
        case Nil => acc
        case x :: xs => loop(xs, x :: acc)
    
    loop(this, Nil)

  def ::[B >: A](a: B): List[B] = 
    List.::(a, this)

  def :::[B >: A](prefix: List[B]): List[B] = {
    if (this.isEmpty) prefix
    else if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ::: this
  }  

  def map[B](f: A => B): List[B] =
    this match 
      case Nil => Nil
      case x :: xs => f(x) :: xs.map(f)
    

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

  @tailrec
  final def mkString(start: String, sep: String, end: String): String = 
    this match 
      case Nil => start + end
      case c :: Nil => start + c + end  
      case c :: cs => cs.mkString(start + c + sep, sep, end)
  
  override def toString = mkString("List(", ", ", ")")