package kasino

import scala.collection.mutable.Buffer

class KasinoText(opponents: Int, playerName: String) {
  require(opponents >= 1, "there must be more than 2 players in the game")
  require(opponents <= 11, "there are only enough cards for 12 players")

  var board = new Board
  var deck = new Deck(0)
  val g = new Game(opponents, playerName, board, deck)

  //main game loop that ends when atleast one player has 16 or more points
  def newWholeGame() = {
    println("New game started!")
    while (g.gameOn) {
      newGame
      g.changeTurn
      scores
    }
    gameEnd
  }

  //start a new game (a game is a single deck played through)
  def newGame() = {
    println("New round started!")
    g.setup()
    g.dealCards()

    for (i <- 0 until 4) {
      g.returnPlayers.foreach(x => playTurn)
    }
    g.roundEnd()
  }

  //once the game has ended, this method gets called
  //prints out the winner/winners
  def gameEnd() = {
    val winners = g.winners
    val out = if (winners.length == 1) {
      s"The winner is ${winners(0).name}!"
    } else {
      s"The winners are ${winners.map(_.name).mkString(",")}"
    }
    println(out)
  }

  def scores = {
    val scores = g.getPlayerScores
    println("Current scores: " + scores.mkString(" ") + "\n")
  }
  
  def playTurn() = {
    val turn = g.turn
    val player = g.returnPlayers(turn) //the player in turn
    val in =
      if (turn == 0) { //if turnIndex is 0 == it's a human player

        val text = s"Cards on the table are: ${g.returnCards.map(_.toString()).mkString(", ")}\n" +
          s"Your cards are: ${player.returnHand.map(_.toString()).mkString(", ")}"
        val text2 = "Your choice: "
        def text3: String = s"Your choice (0-${player.handSize - 1}): "
        println(text)

        var ok = true
        var output = 0
        do {
          val out = if (ok) {
            readLine(text2)
          } else {
            readLine(text3)
          }
          ok = (!out.isEmpty) && (out < player.handSize.toString) && (out >= "0")
          if (ok) output = out.toInt
        } while (!ok)
        output

      } else { //otherwise it's a bot and decides the card without extra info
        g.botPlayCard()
      }

    val card = g.playerCard(in)
    var gotSelection = false
    val choices = g.playCard(in)
    val choice =
      if (choices.flatten.isEmpty) { //if nothing can be picked up...
        Buffer()
        
      } else if (choices.size == 1) { //if there is only one choice..
        choices(0)
        
      } else { //if there is a choice that has to be made...
        val selection = if (turn == 0) { //ask the player which cards they want

          val text = "choices: " + choices.map(_.map(_.toString)) + "\nYour choice: "
          var ok = true
          var output = 0
          do {
            val out = readLine(text)
            ok = (!out.isEmpty) && (out < choices.size.toString) && (out >= "0")
            if (ok) output = out.toInt
          } while (!ok)
          output
          
        } else { //or allow the bot to select
          player.decideSelection(choices)
        }

        //the cards are then removed from the board and added to the players pile
        g.pickupCards(choices(selection))
      }

    //if the player picked something up, they're the player who last picked something up
    if (!choice.isEmpty) g.changeLast

    val part3 = if (g.mokki) s"\n${player.name} clears the board!" else ""//check if the board was cleared
    g.changeTurn //change turn

    //text for card pickup
    val part1 = player.name + " plays: " + card.toText
    val part2 = if (!choice.isEmpty) { ", gets: " + choice.map(_.toString).mkString(", ") } else ""
    println(part1 + part2 + part3)
  }

}
