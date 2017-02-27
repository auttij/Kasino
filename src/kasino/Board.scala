package kasino

import scala.collection.mutable.Buffer

//represents the game board in a cardgame where cards can be played.
object Board {

  var cards = Buffer[Card]() //Contains all the cards in the deck; Turn into private when done testing!

  //clear the board of all cards. Used when resetting the Board state in the beginning of a new game
  def clear(): Unit = cards.clear

  //add a single card to the Board
  private def addCard(c: Card): Unit = cards += c

  //add a list of cards to the board. Mostly used when adding cards to the board in the begining of a round.
  def addCards(list: Seq[Card]): Unit = list.foreach(addCard)

  //collect all of the cards from the Board and return them. 
  //used at the end of a game, after the last player has played his card.
  //the cards are returned to the player who last collected cards from the table
  def collectAll(): Buffer[Card] = {
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

    temp = temp.filter(x => x.tableValue <= c.handValue)
    findCombinations(c, (0, Buffer()), temp, combinations)
    combineCombinations(combinations)
  }

  //finds the possible combinations of cards that can be picked up from the board, that amount to the given card
  //c is the played card,
  //temp contains a  pair that consists of an Integer, that keeps track of the total sum of cards in the Buffer, 
  //that contains a combination of cards that are being tested
  //in contains the remaining cards that can still be added to the sum
  //out contains all the outputs in separate Buffers
  private def findCombinations(c: Card, temp: (Int, Buffer[Card]), in: Buffer[Card], out: Buffer[Buffer[Card]]): Unit = {
    if (temp._1 > c.handValue) {
      //If the total sum of the card combination is higher than the played card, discard it

    } else if (temp._1 == c.handValue) {
      out += temp._2 //if the total sum of the card combination is equal to the played card, add it to the possible combinations

    } else {
      val tempIn: Buffer[Card] = Buffer() //create a Buffer
      in.foreach(x => tempIn += x) //..and fill it with the values of the remaining cards so it can be edited
      in.zipWithIndex //zip the remaining cards with index to be able to drop <index> amount of cards later
        .foreach(x => //recursively call findCombinations for each element
          findCombinations(c,
            (temp._1 + x._1.tableValue, (Buffer() ++= temp._2) += x._1),
            tempIn.drop(x._2 + 1), //new remaining cards is old with index + 1 amount of cards dropped
            out)) //output remains the same
    }
  }

  private def combineCombinations(in: Buffer[Buffer[Card]]): Buffer[Buffer[Card]] = {
    var out = Buffer[Buffer[Card]]()
    in.foreach(x => out += combineBuffers(x, in))
    out.map(_.sortWith(_.toString < _.toString)).toSet.toBuffer
  }

  private def combineBuffers(in: Buffer[Card], left: Buffer[Buffer[Card]]): Buffer[Card] = {
    if (left.size == 0) {
      in
    } else {
      val temp = if (in.intersect(left(0)).isEmpty) in.union(left(0)) else in
      combineBuffers(temp, left.drop(1))
    }
  }

}