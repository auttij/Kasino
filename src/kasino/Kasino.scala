package kasino

import scala.collection.mutable.Buffer

class Kasino(opponents: Int, playerName: String) {
  require(opponents >= 1, "there must be more than 2 players in the game")
  require(opponents <= 11, "there are only enough cards for 12 players")

  private val deck = new Deck(1)
  private val board = Board
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private var scores: Seq[Int] = Seq.fill(players.length)(0)
  private val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played
  private var gameOver = false
  private var turnIndex = 0 //stepper
  private var lastPickup = 0

  def newWholeGame() = {
    while (!gameOver) {
      println("new round started!")
      newGame
    }
    gameEnd
  }

  private def gameEnd() = {
    val maxScore = scores.max
    val winners = scores.zipWithIndex.filter(_._1 == maxScore).map(_._2)
    if (winners.length == 1) {
      s"The winner is ${players(winners(0)).name}!"
    } else {
      val temp = for (i <- winners) yield (players(i).name)
      s"The winners are ${temp.mkString(",")}"
    }
  }

  private def didGameEnd() = {
    gameOver = scores.max >= 16
  }

  def newGame() = {
    deck.initialize
    deck.shuffle
    board.addCards(deck.deal(4))

    for (i <- 0 until rounds) {
      for (i <- 0 until players.length) {
        players(i).addCards(deck.deal(4))
      }

      
      for (i <- 0 until 4) {
        players.foreach( x => playTurn)
      }
    }
    
    players(lastPickup).addToPile(Board.collectAll)
    players(lastPickup).addToPile(deck.collectAll)
    updatePoints
    println((players.map( _.name ) zip scores).mkString(", "))
  }

  def playTurn() = {
    val player = players(turnIndex)
    val in =
      if (turnIndex == 0) {
        readLine(
            "Cards on the table are: " + board.cards.map( _.toString()).mkString(", ") + "\n" + 
            "Your cards are: " + player.hand.map( _.toString()).mkString(", ") + "\n").toInt
      } else {
        player.decideCard
      }
    val card = player.playCard(in)
    val choices = Board.playCard(card)
    
    val choice = if (choices.isEmpty) {
      Board.addCards(Seq(card))
      Buffer[Card]()
      
    } else if (choices.size == 1) {
      val cards = Board.removeCards(choices(0))
      player.addToPile(cards)
      cards
      
    } else {
      val choice =  if (turnIndex == 0) {
        readLine("choices: " + choices.map( _.map( _.toString)) + "\n Your choice: " ).toInt
      } else {
        player.decideSelection(choices)
      }
      val cards = Board.removeCards(choices(choice))
      player.addToPile(cards)
      cards
    }
    
    if (!choice.isEmpty) lastPickup = turnIndex
    
    changeTurn
    println(card.toText + ", " + choice.map( _.toString).mkString(", "))
  }

  def changeTurn() = {
    turnIndex = (turnIndex + 1) % players.length
  }

  private def updatePoints() = {
    val points = for (i <- players) yield i.getPoints
    val mostSpades = points.map(_._2).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Spades
    val mostCards = points.map(_._3).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Cards
    scores = for {
      i <- points.zipWithIndex;
      val x: Int = i match {
        case i if (i._2 == mostSpades & i._2 == mostCards) => 3 + scores(i._2) + i._1._1
        case i if (i._2 == mostSpades) => 2 + scores(i._2) + i._1._1
        case i if (i._2 == mostCards) => 1 + scores(i._2) + i._1._1
        case _ => scores(i._2) + i._1._1
      }
    } yield x
  }
}