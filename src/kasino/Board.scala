package kasino

import scala.collection.mutable.Buffer

//represents the game board in a cardgame where cards can be played.
object Board {

  var cards = Buffer[Card]() //Contains all the cards in the deck; Turn into private when done testing!

  //clear the board of all cards. Used when resetting the Board state in the beginning of a new game
  private def clear() = cards.clear

  //add a single card to the Board
  private def addCard(c: Card) = cards += c

  //add a list of cards to the board. Mostly used when adding cards to the board in the begining of a round.
  def addCards(list: Seq[Card]): Unit = list.foreach(addCard)

  //collect all of the cards from the Board and return them. 
  //used at the end of a game, after the last player has played his card.
  //the cards are returned to the player who last collected cards from the table
  def collectAll() = {
    val temp = Buffer[Card]()
    cards.foreach(c => temp += c)
    cards.clear
    temp
  }

}