package nurekata

enum Suit:
  case Clubs, Diamonds, Hearts, Spades

enum Rank:
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
       Jack, Queen, King, Ace

  def value = ordinal + 2


case class Card(rank: Rank, suit: Suit)

