package nurekata

enum Suit:
  case Clubs, Diamonds, Hearts, Spades

  override def toString = 
    this match 
      case Clubs => "♣"
      case Spades => "♠️"
      case Hearts => "♥️"
      case Diamonds => "♦️"
    

enum Rank:
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
       Jack, Queen, King, Ace

  def value = ordinal + 2

  def >= (that: Rank): Boolean = 
    this.ordinal >= that.ordinal

  override def toString = 
    this match 
      case Ace => "A"
      case King => "K"
      case Queen => "Q"
      case Jack => "J"
      case Ten => "T"
      case n => n.value.toString


case class Card(rank: Rank, suit: Suit):

  override def toString = 
    rank.toString + suit.toString

