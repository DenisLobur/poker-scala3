package nurekata

import nurekata.Suit.*
import nurekata.Rank.*
import nurekata.Option.*
import nurekata.Hand.*

class HandSuite extends munit.FunSuite {
  test("royalFlush") {
    val s10 = Card(Ten, Spades)
    val sJ = Card(Jack, Spades)
    val sQ = Card(Queen, Spades)
    val sK = Card(King, Spades)
    val sA = Card(Ace, Spades)

    val h9 = Card(Nine, Hearts)  
    val h2 = Card(Two, Hearts)  

    val cs = List(h2, s10, sJ, sK, sQ, sA, h9)
    val obtained = royalFlush(cs)
    val expected = Option[RoyalFlush.type](Hand.RoyalFlush)
    assertEquals(obtained, expected)
  }
}
