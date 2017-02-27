package kasino

import scala.collection.mutable.Buffer

//represents the game board in a cardgame where cards can be played.
object Board {

  var cards = Buffer[Card]() //Contains all the cards in the deck; Turn into private when done testing!

  //clear the board of all cards. Used when resetting the Board state in the beginning of a new game
  def clear(): Unit = cards.clear

  //add a single card to the Board
  private def addCard(c: Card): Unit = cards += c
  
  private def addTo[T](coll: Buffer[T], c: T): Unit = coll += c

  //add a list of cards to the board. Mostly used when adding cards to the board in the begining of a round.
  def addCards(list: Seq[Card]): Unit = list.foreach(addCard)

  //collect all of the cards from the Board and return them. 
  //used at the end of a game, after the last player has played his card.
  //the cards are returned to the player who last collected cards from the table
  def collectAll(): Seq[Card] = {
    val temp = Buffer[Card]()
    cards.foreach(c => temp += c)
    cards.clear
    temp
  }

  //plays a card to the game board.
  //returns a Buffer that contains Buffers, that contain selections of cards that can be picked up.
  def playCard(c: Card) = {
    var temp = Buffer[Card]()
    cards.foreach(c => temp += c)
    var combinations = Buffer[Buffer[Card]]()
    
    temp = temp.filter(  x => x.tableValue <= c.handValue )
    //temp.foreach( x => if(x.tableValue == c.handValue) combinations += Buffer(x) )
    //temp = temp.filter(  x => x.tableValue != c.handValue )
    
    println("pöydällä olevat kortit: " + cards)
    println("pelattu kortti: " + c)
    findCombinations(c, (0,Buffer()), temp, combinations)
    println("mahdolliset kombinaatiot: " + combinations)
  }
  
  private def findCombinations(c: Card, temp: (Int, Buffer[Card]), in: Buffer[Card], out: Buffer[Buffer[Card]]): Unit = {
     if (temp._1 > c.handValue) {
       
     } else if (temp._1 == c.handValue) {
      out += temp._2
      
    } else {
      val tempIn: Buffer[Card] = Buffer()
      in.foreach(x => tempIn += x)
      in.zipWithIndex.foreach(x => findCombinations(c, 
                                                    (temp._1 + x._1.tableValue, (Buffer() ++= temp._2) += x._1), 
                                                    tempIn.drop(x._2 + 1), 
                                                    out))
    }
  }
  
}