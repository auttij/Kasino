package kasino

object Test extends App {
  
  val deck = new Deck(1)
  deck.initialize
  deck.shuffle()
  
  val board = Board
  board.clear
  board.addCards(deck.deal(8))
  //board.cards
  
   board.playCard(new Card(7, Spades))
   
}