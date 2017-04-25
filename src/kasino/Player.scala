package kasino

import scala.collection.mutable.Buffer
import scala.util.Random

//Represents a player in a game of casino.
//Can either be a human, or a bot player.
abstract class Player(val name: String) {

  //contains 0-4 cards that are in the players hand.
  private val hand = Buffer[Card]()
  
  //returns the card in the players hand
  def returnHand = this.hand
  
  //tells the size of the hand
  def handSize = hand.size

  //contains the cards the player has collected. Used when calculating points at the end of a round.
  private val pile = Buffer[Card]()
  
  //returns the contents of the players pile, as strings. Used when saving
  def returnPile = this.pile.map( _.toString)

  def decideCard: Int
  def decideSelection(in: Buffer[Buffer[Card]]): Int
  
  //add cards to the players pile
  def addToPile(in: Buffer[Card]) = {
    pile ++= in
    in
  }

  //Clear the players pile and return it's contents
  def emptyPile(): Buffer[Card] = {
    val temp = for (i <- pile) yield i
    pile.clear
    temp
  }

  def playCard(index: Int): Card = hand.remove(index)

  def addCard(in: Card) = hand += in
  def addCards(in: Buffer[Card]) = in.foreach(addCard)
  
  def getPoints: (Int, Int, Int) = {
    val cards = this.emptyPile()
    val points = cards.map( _.pointValue).sum
    val spadesCount = cards.filter( _.suit ==  Spades ).length
    val cardCount = cards.length
    (points, spadesCount, cardCount)
  }
  
}

class HumanPlayer(name: String) extends Player(name) {
    def decideCard = new Random().nextInt(this.handSize)  //never actually used
    def decideSelection(in: Buffer[Buffer[Card]]) = new Random().nextInt(in.size) //never actually used
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
      "Tomi Bot"
      )
      
  def apply(i: Int) = new Bot(names(i))
}