package kasino

import scala.collection.mutable.Buffer
import scala.util.Random

//Represents a player in a game of casino.
//Can either be a human, or a bot player.
abstract class Player(val name: String) {

  //contains 0-4 cards that are in the players hand.
  val hand = Buffer[Card]()
  
  //tells the size of the hand for Bots
  def handSize = hand.length

  //contains the cards the player has collected. Used when calculating points at the end of a round.
  private val pile = Buffer[Card]()

  def decideCard: Int
  def decideSelection(in: Buffer[Buffer[Card]]): Int
  
  def addToPile(in: Buffer[Card]) = {
    pile ++= in
    in
  }

  def emptyPile(): Buffer[Card] = {
    val temp = Buffer[Card]()
    temp ++= pile
    pile.clear
    temp
  }

  def playCard(c: Card): Card = hand.remove(hand.indexOf(c))

  def playCard(index: Int): Card = hand.remove(index)

  def addCards(in: Buffer[Card]) = hand ++= in
  
  def getPoints: (Int, Int, Int) = {
    val cards = this.emptyPile()
    val points = cards.map( _.pointValue).sum
    val spadesCount = cards.filter( _.suit ==  Spades ).length
    val cardCount = cards.length
    (points, spadesCount, cardCount)
  }
  
}

class HumanPlayer(name: String) extends Player(name) {
    def decideCard = new Random().nextInt(this.handSize)
    def decideSelection(in: Buffer[Buffer[Card]]) = new Random().nextInt(in.size)
}

class Bot(name: String) extends Player(name) {
    val rand = new Random()
    def decideCard = new Random().nextInt(this.handSize)
    
    def decideSelection(in: Buffer[Buffer[Card]]) = {
      in.zipWithIndex.maxBy( _._1.map( _.pointValue).sum )._2
    }
}

object Bot {
  val names: Buffer[String] =
    Buffer(
      "Kasa Bot",
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
      "Paavo Bot"
      )
      
  def apply(i: Int) = new Bot(names(i))
}