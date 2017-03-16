package kasino

import scala.collection.mutable.Buffer

class Kasino(opponents: Int, playerName: String) {
  require(opponents >= 1, "there must be more than 2 players in the game")
  require(opponents <= 11, "there are only enough cards for 12 players")

  private val deck = new Deck(0)
  private val board = Board
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private var scores: Seq[Int] = Seq.fill(players.length)(0)
  private val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played
  private var gameOver = false
  private var turnIndex = 0 //stepper
  private var lastPickup = 0

  def newWholeGame() = {
    while (!gameOver) {
      println("new game started!")
      newGame
      changeTurn
    }
    gameEnd
  }

  private def gameEnd() = {
    val maxScore = scores.max
    val winners = scores.zipWithIndex.filter(_._1 == maxScore).map(_._2)
    val out = if (winners.length == 1) {
      s"The winner is ${players(winners(0)).name}!"
    } else {
      val temp = for (i <- winners) yield (players(i).name)
      s"The winners are ${temp.mkString(",")}"
    }
    
    println(out)
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
    didGameEnd
    println((players.map( _.name ) zip scores).mkString(", "))
  }

  def playTurn() = {
    val player = players(turnIndex)
    val in =
      if (turnIndex == 0) {
        readLine(
            "Cards on the table are: " + board.cards.map( _.toString()).mkString(", ") + "\n" + 
            "Your cards are: " + player.hand.map( _.toString()).mkString(", ") + "\n" +
            "Your choice: ").toInt
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
        readLine("choices: " + choices.map( _.map( _.toString)) + "\nYour choice: " ).toInt
      } else {
        player.decideSelection(choices)
      }
      val cards = Board.removeCards(choices(choice))
      player.addToPile(cards)
      cards
    }
    
    if (!choice.isEmpty) lastPickup = turnIndex
    mokki
    
    changeTurn
    val part1 = player.name + " plays: " +card.toText
    val part2 = if (!choice.isEmpty) { ", gets: "  + choice.map( _.toString).mkString(", ") } else ""
    println(part1 + part2)
  }

  def mokki() = {
    if (Board.cards.isEmpty) {
      scores = for {i <- scores.zipWithIndex 
        val x: Int = i match {
          case i if (i._2 == turnIndex) => i._1 + 1
          case _ => i._1
        }
      } yield x
      println(players(turnIndex).name + " gets a mökki.")
    }
  }
  
  def changeTurn() = {
    turnIndex = (turnIndex + 1) % players.length
  }

  private def updatePoints() = {
    val points = for (i <- players) yield i.getPoints
    val mostSpades = points.map(_._2).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Spades
    val mostCards = points.map(_._3).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Cards
    val temp = for (i <- points.zipWithIndex) yield (scores(i._2) + i._1._1)
    
    scores = for {
      i <- temp.zipWithIndex;
      val x: Int = i match {
        case i if (i._2 == mostSpades & i._2 == mostCards & i._2 == lastPickup) => 4 + temp(i._2)
        case i if (i._2 == mostCards & i._2 == lastPickup) => 2 + temp(i._2)
        case i if (i._2 == mostSpades & i._2 == lastPickup) => 3 + temp(i._2)
        case i if (i._2 == mostSpades & i._2 == mostCards) => 3 + temp(i._2)
        case i if (i._2 == mostSpades) => 2 + temp(i._2)
        case i if (i._2 == mostCards) => 1 + temp(i._2)
        case i if (i._2 == lastPickup) => 1 + temp(i._2)
        case _ => temp(i._2)
      }
    } yield x

  }
}
