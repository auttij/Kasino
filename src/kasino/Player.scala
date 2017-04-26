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

  //decide a card that will be play from the players hand  
  def decideCard: Int
  
  //decides a combination of cards that the player wants to pick up after they have played a card.
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

  //plays a card with the given index from the players hand.
  def playCard(index: Int): Card = hand.remove(index)

  //Adds a single card to the players hand
  def addCard(in: Card) = hand += in
  
  //Adds a collection of  cards to the players hand
  def addCards(in: Buffer[Card]) = in.foreach(addCard)
  
  //clears the players Pile and calculates values based on its contents,
  //calcultaed values are returned as a 3-Int tuple. The values are as follows:
  //The sum of points of the cards in the pile (each ace would grow this number by 1 and so on)
  //The amount of spades, that the player has collected.
  //The amount of cards, that the player has collected.
  def getPoints(): (Int, Int, Int) = {
    val cards = this.emptyPile()
    val points = cards.map( _.pointValue).sum
    val spadesCount = cards.filter( _.suit ==  Spades ).length
    val cardCount = cards.length
    (points, spadesCount, cardCount)
  }
  
}

//this sub-class should probably be removed and integrated to the regular player class.
class HumanPlayer(name: String) extends Player(name) {
    def decideCard = new Random().nextInt(this.handSize)  //never actually used
    def decideSelection(in: Buffer[Buffer[Card]]) = new Random().nextInt(in.size) //never actually used
}

//represents a bot player in the game.
class Bot(name: String) extends Player(name) {
    private val rand = new Random()
    //chooses which card to play.
    //currently selects a random card.
    def decideCard = new Random().nextInt(this.handSize) 
    
    //when given multiple choices that the Bot can choose from, select which one to play
    def decideSelection(in: Buffer[Buffer[Card]]) = {  //Currently chooses the one with most points in it.
      in.zipWithIndex.maxBy( _._1.map( _.pointValue).sum )._2
    }
}

object Bot {
  val names: Buffer[String] =  //Names for each bot player
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