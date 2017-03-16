package kasino

object Test extends App {

  val deck = new Deck(2)
  deck.initialize
  deck.shuffle()
  
  val cards = deck.deal(8)
  val c = new Card(7, Spades)
  
  val board = Board
  board.clear
  board.addCards(cards)

  println("played card: " + c + s"  (tableValue = ${c.tableValue})")
  println("cards: " + cards)
  
  val choices = board.playCard(c)
  println("choices: " + choices)
  println(choices.map(_.map(_.toText)))
  
  board.removeCards(choices(0))
  println(board.cards)
  println(board.cards.map( _.toText))
  
}