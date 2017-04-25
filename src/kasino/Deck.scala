package kasino

import scala.collection.mutable.Buffer
import scala.util.Random

//Represents a card of playing Cards. Starts out empty and needs to be initialized to add 1 of each card.
class Deck(seed: Int) {
  //Creates a random for the shuffling to use. Can use a seed but it's for testing purposes
  private val myRand = if (seed == 0) new Random() else new Random(seed)

  //Contains all the cards in the deck; Turn into private when done testing!
  private var cards = Buffer[Card]()

  //Contains all the suits. Used by initialize to go through all the suits to make a complete deck.
  private val Suits = Vector[Suit](Spades, Diamonds, Clubs, Hearts)
  
  //adds a card to the cards buffer
  private def addCard(value: Int, Suit: Suit) = {
    cards += new Card(value, Suit)
  }
  
  //adds a collection of cards to the deck, used when loading
  def addCards(in: Seq[Card]) = in.foreach( x => cards += x)

  //Creates a new deck that contains all the suits going ace -> two -> ... -> king
  //Suits are in order, defined by the Suits-vector
  def initialize() = {
    cards.clear

    for (s <- Suits.indices) { //go through suits
      for (v <- 1 to 13) { //go through all values 1-13
        addCard(v, Suits(s)) //add card
      }
    }
  }

  //Shuffles the deck
  def shuffle() = cards = myRand.shuffle(cards)

  //deal a certain amount of cards from the deck
  def deal(amount: Int): Buffer[Card] = {
    if (amount > cards.size) { //if there aren't enought cards in the deck, throw an exception
      throw new IllegalArgumentException("tried to deal too many cards from deck.")
    }

    if (amount < 0) { //if the amount is negative, throw an exception
      throw new IllegalArgumentException("tried to deal a negative amount of cards from deck.")
    }

    val dealt = cards.slice(0, amount) //create a new Buffer with the first <amount> cards from the cards-buffer
    cards = cards.drop(amount) //remove <amount> cards from the cards-buffer
    dealt //return the removed cards
  }

  def collectAll(): Buffer[Card] = {
    val temp = for (i <- cards) yield i
    cards.clear
    temp
  }
  
  def isEmpty = this.cards.isEmpty
  
  //returns all the cards as text in the deck without changing the actual Buffer storing them.
  def returnCards = this.cards.map(_.toString)
}