package kasino

object Test extends App {

//  val deck = new Deck(2)
//  deck.initialize
//  deck.shuffle()
//  
//  val cards = deck.deal(8)
//  val c = new Card(7, Spades)
//  
//  val board = Board
//  board.clear
//  board.addCards(cards)
//
//  println("played card: " + c + s"  (tableValue = ${c.tableValue})")
//  println("cards: " + cards)
//  
//  val choices = board.playCard(c)
//  println("choices: " + choices)
//  
//  val pelaaja = new HumanPlayer("testi")
//  pelaaja.addToPile(board.removeCards(choices(0)))
//  println(board.cards)
//  
//  println(pelaaja.getPoints)
//  
//  val kasino = new Kasino(8, "falarikae")
////  println(kasino.players.map(_.name))
////  println(kasino.rounds)
//
  
  val kasino = new Kasino(3, "falarikae")
  kasino.newWholeGame()
  
  }