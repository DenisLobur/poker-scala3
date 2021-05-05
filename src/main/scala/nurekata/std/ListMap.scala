package nurekata.std

import nurekata.Option
import nurekata.List

case class ListMap[K, +V](ns: List[(K, V)]) extends Map[K, V]:
   override def get(key: K): Option[V] =
      ns.find((k, _) => k == key)
        .map(_._2)

   override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = 
      ListMap(kv :: ns.filter(_._1 != kv._1))

   override def - (key: K) = 
      ListMap(ns.filter(_._1 != key))

   override def keys = 
      ns.map(_._1)

   override def values = 
      ns.map(_._2)

   override def toList = ns
