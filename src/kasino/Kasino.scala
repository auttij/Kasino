package kasino

import scala.collection.mutable.Buffer

class Kasino(opponents: Int, playerName: String) {
  require(opponents >= 1, "there must be more than 2 players in the game")
  require(opponents <= 11, "there are only enough cards for 12 players")

  private val deck = new Deck(0)
  private val board = Board
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private val scores = Scores(this, players.size)
  private val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played
  private var gameOver = false
  private var turnIndex = 0 //stepper
  private var lastPickup = 0

  def returnPlayers = this.players
  def returnLastPickup = this.lastPickup

  //main game loop that ends when atleast one player has 16 or more points
  def newWholeGame() = {
    while (!gameOver) {
      println("new game started!")
      newGame
      changeTurn
    }
    gameEnd
  }

  //changes the player who is currently playing to the next player
  def changeTurn() = {
    turnIndex = (turnIndex + 1) % players.length
  }

  //did the game end? if so gameOver = true
  private def didGameEnd() = {
    if (scores.isGameOver) gameOver = true
  }

  //once the game has ended, this method gets called
  //prints out the winner/winners
  private def gameEnd() = {
    val winners = scores.getWinners
    val out = if (winners.length == 1) {
      s"The winner is ${players(winners(0)).name}!"
    } else {
      val temp = for (i <- winners) yield (players(i).name)
      s"The winners are ${temp.mkString(",")}"
    }

    println(out)
  }

  //start a new game (a game is a single deck played through)
  def newGame() = {
    deck.initialize
    deck.shuffle
    board.addCards(deck.deal(4))

    for (i <- 0 until rounds) {
      for (i <- 0 until players.length) {
        players(i).addCards(deck.deal(4))
      }

      for (i <- 0 until 4) {
        players.foreach(x => playTurn)
      }
    }
    roundEnd
  }

  //clearing the board and deck once the deck has been played through
  def roundEnd() = {
    players(lastPickup).addToPile(Board.collectAll)
    players(lastPickup).addToPile(deck.collectAll)
    scores.updatePoints
    didGameEnd
    println(scores.scoresWithIndex.map(x => (players(x._1).name, x._2)).mkString(", "))
  }

  def playTurn() = {
    val player = players(turnIndex) //the player in turn
    val in =
      if (turnIndex == 0) { //if turnIndex is 0 == it's a human player
        readLine(
          "Cards on the table are: " + board.cards.map(_.toString()).mkString(", ") + "\n" +
            "Your cards are: " + player.hand.map(_.toString()).mkString(", ") + "\n" +
            "Your choice: ").toInt
      } else { //otherwise it's a bot and decides the card without extra info
        player.decideCard
      }

    val card = player.playCard(in) //the card that will be played
    val choices = Board.playCard(card) //creates choices of what can be picked up

    val choice =
      if (choices.isEmpty) {  //if nothing can be picked up...
        Board.addCards(Seq(card))  //play the card to the table
        Buffer[Card]()

      } else if (choices.size == 1) {  //if there is only one choice..
        val cards = Board.removeCards(choices(0))  //the card that can be picked up are automatically picked up..
        player.addToPile(cards)  //and added to the players pile
        cards

      } else {  //if there is a choice that has to be made...
        val choice = if (turnIndex == 0) {  //ask the player which cards they want
          readLine("choices: " + choices.map(_.map(_.toString)) + "\nYour choice: ").toInt
        } else {  //or allow the bot to select
          player.decideSelection(choices)
        }
        
        //the cards are then removed from the board and added to the players pile
        val cards = Board.removeCards(choices(choice))
        player.addToPile(cards)
        cards
      }
    
    //if the player picked something up, they're the player who last picked something up
    if (!choice.isEmpty) lastPickup = turnIndex
    mokki  //check if the board was cleared

    changeTurn  //change turn
    //placeholder text so the game is easier to play without a GUI
    val part1 = player.name + " plays: " + card.toText
    val part2 = if (!choice.isEmpty) { ", gets: " + choice.map(_.toString).mkString(", ") } else ""
    println(part1 + part2)
  }

  //checks if the board has been cleared and if so, gives the player who cleared it a point
  def mokki() = {
    if (Board.cards.isEmpty) {
      scores.addOne(turnIndex)
      println(players(turnIndex).name + " gets a mökki.")
    }
  }

}
