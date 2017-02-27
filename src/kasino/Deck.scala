package kasino

import scala.collection.mutable.Buffer
import scala.util.Random

class Deck(seed: Int) {
  //Creates a random for the shuffling to use. Can use a seed but it's for testing purposes
  private val myRand = if (seed == 0) new Random() else new Random(seed) 
  
  //Contains all the cards in the deck; Turn into private when done testing!
  var cards = Buffer[Card]() 
  
  //Contains all the suits. Used by initialize to go through all the suits to make a complete deck.
  private val Suits = Vector[Suit](Spades, Diamonds, Clubs, Hearts)

  //adds a card to the cards buffer
  private def addCard(value: Int, Suit: Suit) = {
    cards += new Card(value, Suit)
  }
  
  //Creates a new deck that contains all the suits going ace -> two -> ... -> king
  //Suits are in order, defined by the Suits-vector
  def initialize() =  {
    cards = Buffer[Card]()
    
    for (s <- Suits.indices) { //go through suits
      for (v <- 1 to 13) {     //go through all values 1-13
        addCard(v,Suits(s))    //add card
      }
    }
  }
  
  //Shuffles the deck
  def shuffle() = cards = myRand.shuffle(cards)
  
  //deal a certain amount of cards from the deck
  def deal(amount: Int): Seq[Card] = {
    if (amount > cards.size) {           //if there aren't enought cards in the deck, throw an exception
      throw new IllegalArgumentException("tried to deal too many cards from deck.")
    }
    
    if (amount > 0) {                    //if the amount is negative, throw an exception
      throw new IllegalArgumentException("tried to deal a negative amount of cards from deck.")
    }
    
    val dealt = cards.slice(0, amount)   //create a new Buffer with the first <amount> cards from the cards-buffer
    cards = cards.drop(amount)           //remove <amount> cards from the cards-buffer
    dealt                                //return the removed cards
  }
}