package nurekata

import nurekata.Rank.*
import nurekata.Rank
import nurekata.Suit.*
import nurekata.Suit
import nurekata.List.*
import nurekata.std.*



@main def hello: Unit = {
   // println("Hello world!")
   // println(msg)

val rfCards = Card(Ten, Spades) :: Card(Queen, Spades)
:: Card(Two, Hearts) :: Card(King, Spades) :: Card(Ace, Spades)
:: Card(Jack, Spades) :: Card(Ten, Hearts) :: Nil

println(s"current hand: $rfCards")

given sortOrd: Ordering[Card] = (x, y) => y.rank.ordinal.compare(x.rank.ordinal)
val sortedRF = rfCards.sorted
println(s"sorted hand: $sortedRF")

val hasRoyalFlush = royalFlush(rfCards)
println(s"hand has royalFlush: $hasRoyalFlush")

val strFl = Card(Nine, Diamonds) :: Card(Eight, Diamonds)
:: Card(Seven, Hearts) :: Card(Six, Diamonds)
:: Card(Five, Diamonds) :: Card(Nine, Spades)
:: Card(Four, Diamonds) :: Nil

// val hasStraightFlush = straightFlush(strFl)
// println(s"hand has straightFlush: $hasStraightFlush")
}

def msg = "I was compiled by Scala 3. :)"


