package nurekata

import nurekata.List.*
import nurekata.Option.*
import nurekata.Suit.*

type Cards = List[Card]

object Cards: 
  
  def apply(s: String): Option[Cards] = 
    parse(s.split(" ").toList)

  def parse(ss: List[String]): Option[Cards] = 
    ss match 
      case Nil => Some(Nil)
      case s :: ss => 
        parse(ss)
          .flatMap(cs => 
            parseCard(s)
             .map(c => c :: cs)
          )

  def parseCard(s: String): Option[Card] = 
    if s.length != 2 then None 
    else 
      parseRank(s.head.toString)
        .flatMap(r => 
          parseSuit(s.tail)
            .map(s => Card(r, s)))

  def parseSuit(s: String): Option[Suit] = 
    s match 
      case "♣" => Some(Clubs)
      case "♠️" => Some(Spades)
      case "♥️" => Some(Hearts)
      case "♦️" => Some(Diamonds)
      case _ => None

  def parseRank(s: String): Option[Rank] = 
    ???


extension [T](a: Array[T])
  def toList: List[T] = ???
    
    