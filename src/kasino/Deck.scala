package kasino

import scala.collection.mutable.Buffer

class Deck {
  var cards = Buffer[Card]()
  
  
  private val Suits = Vector[Suit](Spades, Diamonds, Clubs, Hearts)

  def addCard(value: Int, Suit: Suit) = {
    cards += new Card(value, Suit)
  }
  
  def initialize() =  {
    cards = Buffer[Card]()
    
    for (s <- Suits.indices) {
      for (v <- 1 to 13) {
        addCard(v,Suits(s))
      }
    }
  }
  
}