import nurekata.Suit.*
import nurekata.Rank.*
import nurekata.List.*
import nurekata.*


val s10 = Card(Ten, Spades)
val sJ = Card(Jack, Spades)
val sQ = Card(Queen, Spades)
val sK = Card(King, Spades)
val sA = Card(Ace, Spades)

val h9 = Card(Nine, Hearts)


val cs = Card(Ace, Spades) :: Card(Nine, Spades) ::
   Card(Ten, Spades) :: Nil

val cards = Card(Two, Hearts) :: cs

filterGte10(cards)
sameSuit(cs)

cards.length
hasRoyalFlush(cards)

val rf = Card(Ten, Spades) :: Card(Queen, Spades) :: Card(Two, Hearts) :: Card(King, Spades) :: Card(Ace, Spades) ::
 Card(Jack, Spades) :: Card(Five, Hearts) :: Nil

hasRoyalFlush(rf)

val rs = ranks(cards)
cards
cards.splitAt(2)
sorted(rs)
cards.zip(5 :: 4 :: 3 :: Nil)

cards.zip(cards.tail)

cards
cards.zip(cards.tail.tail)

cards.drop(5)
cards.reverse

straight(rf)
straight(Card(Five, Spades) :: Card(Four, Diamonds)
  :: Card(Ace, Diamonds) :: Card(Two, Hearts)
  :: Card(Three, Hearts) :: Card(Three, Diamonds) :: Nil)
// 2 3 4 5 6 7 8
   // 6 7 8 
   // (2, 6), (3, 7) ( 4, 8)







rf
straight(Card(Nine, Clubs) :: rf)
straight(rf)
straight(cards)