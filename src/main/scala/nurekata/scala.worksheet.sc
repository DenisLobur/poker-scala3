import nurekata.Suit.*
import nurekata.Rank.*
import nurekata.Cards.*
import nurekata.*
  

val cs = Cons(Card(Ace, Spades), Cons(Card(Nine, Spades), Cons(Card(Ten, Spades), Nil)))

val cards = Cons(Card(Two, Hearts), cs)

filterGte10(cards)

sameSuit(cards)
sameSuit(cs)

cards.length
hasRoyalFlush(cards)

val rf = 
Cons(Card(Ten, Spades),
  Cons(Card(Queen, Spades),
    Cons(Card(Two, Hearts),
      Cons(Card(King, Spades),
        Cons(Card(Ace, Spades),
          Cons(Card(Jack, Spades),
            Cons(Card(Five, Hearts),
              Nil)
              )
            )
          )
        )
      )
    )
hasRoyalFlush(rf)
