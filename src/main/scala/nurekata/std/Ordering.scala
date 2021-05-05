package nurekata.std

trait Ordering[A]:
   def compare(x: A, y: A): Int

   def reverse: Ordering[A] = (x, y) => compare(y, x)

extension [A](x: A)(using ord: Ordering[A])
   def >(y: A): Boolean = 
       ord.compare(x, y) > 0
