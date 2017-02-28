package kasino

import scala.collection.mutable.Buffer

//Represents a player in a game of casino.
//Can either be a human, or a bot player.
abstract class Player(val name: String) {

  //contains 0-4 cards that are in the players hand.
  private var hand = Buffer[Card]()

  //contains the cards the player has collected. Used when calculating points at the end of a round.
  private var pile = Buffer[Card]()

  def addToPile(in: Buffer[Card]) = { pile ++= in }

  def emptyPile(): Buffer[Card] = {
    val temp = Buffer[Card]()
    temp ++= pile
    pile.clear
    temp
  }

  def playCard(c: Card): Card = hand.remove(hand.indexOf(c))

  def playCard(index: Int): Card = hand.remove(index)

  def addCards(in: Buffer[Card]) = hand ++= in
}

class HumanPlayer(name: String) extends Player(name) {

}

class Bot(name: String) extends Player(name) {

}

object Bot {
  val names: Buffer[String] =
    Buffer("Kasa Bot",
      "Kyösti Bot",
      "Julius Bot",
      "Alex Bot",
      "Elisa Bot",
      "Liisi Bot",
      "Sofia Bot",
      "Tuomas Bot",
      "Mia Bot",
      "AP Bot",
      "Niilo Bot",
      "Petra Bot",
      "Tomi Bot",
      "Paavo Bot")

  
      
}