package nurekata.std

import nurekata.Option

trait Map[K, +V]:
   def get(key: K): Option[V]

   def getOrElse[V1 >: V](key: K, default: => V1): V1 =
      get(key).getOrElse(default)

   def +[V1 >: V](kv: (K, V1)): Map[K, V1]

   def -(k: K): Map[K, V]

   def values: List[V]

   def keys: List[V]

   def toList: List[(K, V)]

object Map:
   def empty[K, A]: Map[K, A] = ???

