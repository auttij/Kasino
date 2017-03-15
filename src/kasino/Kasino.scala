package kasino

import scala.collection.mutable.Buffer

class Kasino(opponents: Int, playerName: String) {
  require(opponents >= 1, "there must be more than 2 players in the game")
  require(opponents <= 11, "there are only enough cards for 12 players")

  val deck = new Deck(0)
  val board = Board
  val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private var startingPlayer: Int = 0
  val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played

  def newGame() = {
    deck.initialize
    deck.shuffle

    board.addCards(deck.deal(4))

    players.foreach(_.addCards(deck.deal(4)))
  }

  def caluculatePoints(old: Seq[Int]): Seq[Int] = {
    val points = for (i <- players) yield i.getPoints
    val mostSpades = points.map(_._2).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Spades
    val mostCards = points.map(_._3).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Cards
    for {
      i <- points.zipWithIndex;
      val x: Int = i match {
        case i if (i._2 == mostSpades & i._2 == mostCards) => 3 + old(i._2) + i._1._1
        case i if (i._2 == mostSpades) => 2 + old(i._2) + i._1._1
        case i if (i._2 == mostCards) => 1 + old(i._2) + i._1._1
        case _ => old(i._2) + i._1._1
      }
    } yield x
  }
}