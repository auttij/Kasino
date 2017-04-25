package kasino

import scala.swing._
import scala.swing.event._
import javax.swing.UIManager
import scala.swing.GridBagPanel.Fill
import scala.collection.mutable.Buffer
import javax.swing.border._
import java.awt.Color
import scala.swing.Dialog
import javax.swing.JOptionPane
import javax.swing.Icon
import scala.util.Try

object TextBasedGUI extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  def top = new MainFrame {

    //the display area where played cards and other inputs are shown.
    val log = new TextArea(18, 80) {
      editable = false
      wordWrap = true
      lineWrap = true
      text = { //tutorial text for how to play the game
        "When playing, choose the card you want\n" +
          "to play by entering a value between 0 and 3\n" +
          "that represents the card you want to play.\n\n" +
          "When you play a card, all cards and combinations of cards\n" +
          "that equal to the value of your played card are picked up\n" +
          "from the table and added to your pile.\n" +
          "Win the game by collecting the most points.\n Points are awarded for getting:\n" +
          " -Most cards (1 point)\n" +
          " -Most spades (2 points)\n" +
          " -10 of Diamonds (2 points) (value when played is 16)\n" +
          " -2 of Spades (1 point)  (value when played is 15)\n" +
          " -Any Ace (1 point each) (value when played is 14)\n" +
          "Or by clearing the board. (1 point)\n" +
          "Points are calculated at the end of each round.\n" +
          "The first player to 16 points wins.\n"
      }
    }
    //text area where information about cards on board and in players hand is shown
    val infoBox = new TextArea(3, 80) {
      editable = false
      wordWrap = true
      lineWrap = true
      text = "Start a new game from the menu bar!"
    }
    //text input area
    val input = new TextField(20) {
      minimumSize = preferredSize
    }
    this.listenTo(input.keys)

    // Events: 
    this.reactions += {
      case keyEvent: KeyPressed =>
        if (keyEvent.source == this.input && keyEvent.key == Key.Enter && this.g.gameOn) {

          val command = this.input.text.trim
          if (command.nonEmpty) { //if something was written in the text area

            val isInt = Try(command.toInt).isSuccess //Checks if the input can be turned to and Int       
            if (isInt) {

              val player = g.humanPlayer
              val in = command.toInt
              if (in < player.handSize && in >= 0) {
                playerInput = in //changes the variable used for selecting the card the user plays
                playTurn //plays the players turn
                playUntilPlayer() //plays the game until it's the players turn again
              }
            }

            this.input.text = "" //clear the text area
          }
        }
    }

    //  Layout:
    this.contents = new GridBagPanel {
      import scala.swing.GridBagPanel.Anchor._
      def constraints(x: Int, y: Int,
                      gridwidth: Int = 1, gridheight: Int = 1,
                      weightx: Double = 0.0, weighty: Double = 0.0,
                      fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None,
                      anchor: GridBagPanel.Anchor.Value = NorthWest,
                      insets: Insets = new Insets(5, 5, 5, 5)): Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridwidth = gridwidth
        c.gridheight = gridheight
        c.weightx = weightx
        c.weighty = weighty
        c.fill = fill
        c.anchor = anchor
        c.insets = insets
        c
      }

      add(new Label("Info:"),
        constraints(0, 0, insets = new Insets(8, 5, 5, 5)))
      add(new Label("Command:"),
        constraints(0, 3, insets = new Insets(8, 5, 5, 5)))
      add(new Label("History:"),
        constraints(0, 4, insets = new Insets(8, 5, 5, 5)))
      add(infoBox,
        constraints(1, 0, weightx = 1.0, fill = GridBagPanel.Fill.Horizontal))
      add(input,
        constraints(1, 3, weightx = 1.0, fill = GridBagPanel.Fill.Horizontal))
      add(log,
        constraints(1, 4, gridheight = 3, weighty = 1.0,
          fill = GridBagPanel.Fill.Both))

    }

    // Menu: 
    val startAction = Action("New game") { startGame }
    val loadAction = Action("Load game") { loadGame }
    val saveAndQuit = Action("Save & Quit") { saveQuit }
    val quitAction = Action("Quit") { dispose() }
    saveAndQuit.enabled = (false)

    this.menuBar = new MenuBar {
      contents += new Menu("Program") {
        contents += new MenuItem(startAction)
        contents += new MenuItem(loadAction)
        contents += new MenuItem(saveAndQuit)
        contents += new MenuItem(quitAction)
      }

    }

    // Set up the initial state of the GUI:

    this.title = "Kasino"
    this.location = new Point(700, 400)
    this.minimumSize = new Dimension(400, 400)
    this.pack()
    this.input.requestFocusInWindow()
    this.visible = true

    //Methods that update the text areas of the GUI

    //clears the log of all it's text
    def clearLog() = {
      this.log.text = ""
    }

    //prints out the given string to the log text area
    def updateLog(in: String) = {
      if (g.turn == 1) {
        clearLog
      }
      log.text += in + "\n"
    }

    //updates the infobox with information about cards on the table and cards in the players hand
    def updateInfo = {
      this.infoBox.text = {
        s"Cards on the table: ${g.getCards.mkString(", ")}\n" +
          s"Your hand: ${g.getPlayers(0).returnHand.mkString(", ")}"
      }
    }

    //updates the log with information on who is the current dealer in the game.
    def dealerInfo = {
      val dealer = g.getPlayers(g.getDealer)
      updateLog(s"The current dealer is ${dealer.name}")
    }

    //Updates the log with information about player scores
    def scores = {
      val scores = g.getPlayerScores
      updateLog("Current scores: " + scores.mkString(" ") + "\n")
    }

    //Asks the player for input and creates a new game based on given information
    def startGame = {
      var opponents: Option[String] = None
      var ok = false
      val text = "How many opponents do you want to play against?\nChoose a number between 1 and 11.\nSuggested amounts are 2, 3 and 5."
      do {
        opponents = Dialog.showInput(log, text, initial = "")  //asks the player how many opponents they want to play against
        val isInt = Try(opponents.get.toInt).isSuccess
        ok = if (isInt) (opponents.get.toInt <= 11 && opponents.get.toInt > 0) else false

      } while (!opponents.isDefined || !ok)

      var name: Option[String] = None
      do {
        name = Dialog.showInput(log, "What's your name?", initial = "") //ask the players name
      } while (!name.isDefined)

      g = new Game(opponents.get.toInt, name.get, new Board, new Deck(0)) //create a new game
      enableWithGame()  //enable saving
      clearLog  //clears the log
      newWholeGame()  //starts a new game
    }

    //loads a game from the save file
    def loadGame = {
      try {  //try to load the save data
        g = ChunkIO.load
      } catch {
        case e: CorruptedSaveFileException => {
          clearLog()
          val text = "Loading save data failed!\n" + e
          updateLog(text)
        }
      }
      if (g.gameOn) {
        clearLog
        enableWithGame()
        loadWholeGame
      } else {
        updateLog("The saved game was already over!")
      }
    }

    //Saves the game and closes the program
    def saveQuit = {
      val res = Dialog.showConfirmation(
        log,
        "Are you sure you want to quit?",
        title = "Save & Quit",
        optionType = Dialog.Options.YesNo)

      if (res == Dialog.Result.Yes) {
        ChunkIO.saveGame(g)
        this.dispose()
      }
    }

    //there were supposed to be more items to be enable/disabled, 
    //but like this I could have done without these two methods
    //Enable certain things, called when game starts
    def enableWithGame() = {  
      saveAndQuit.enabled = (true)
    }

    //Disable certain things, called when a game ends
    def disableAfterGame() = {
      saveAndQuit.enabled = (false)
    }

    // Game:
    var g = new Game(5, "The Nameless One", new Board, new Deck(0))
    var playerInput = 0

    //main game loop that ends when atleast one player has 16 or more points
    def newWholeGame() = {
      updateLog("New game started!")
      newGame
      playUntilPlayer
    }

    //starts a game loaded from a save file and show some useful info for the player.
    def loadWholeGame() = {
      updateLog("Game loaded!")
      scores
      updateInfo
    }

    //start a new game
    //sets up the deck, board and players so that a game can be played
    def newGame(): Unit = {
      updateLog("New round started!")
      dealerInfo
      g.newGame()
      updateInfo
    }

    //plays the game automaticall until it's the players turn
    def playUntilPlayer(): Unit = {
      if (g.gameOn && isRoundOver) newGame //if someone hasn't won and no one has cards, start a new game
      if (g.turn != 0) playBots() //if the player in turn isn't human play until it is or game ends
      if (!g.gameOn) gameEnd //if someone has won, end the game
      if (g.gameOn && g.isRoundOver) { //if a round has ended but no one has won
        newGame //start a new game
        playUntilPlayer() //and play until it's the players turn
      }
    }

    //plays turns until the player in turn is a human player
    def playBots() = {
      while (g.turn != 0 && !isRoundOver) {
        playTurn
      }
    }

    //checks if the round is over and returns true/false
    //if the round has ended, it runs all the round ending methods
    def isRoundOver() = {
      if (g.isRoundOver) {
        g.roundEnd() //clear board and count scores
        //tells the player the round has ended and who was the last to pick up any cards
        updateLog(s"Round over! \n${g.asName(g.last)} was last pickup a card and clears the board.")
        scores //prints the scores

        true
      } else {
        false
      }
    }

    //once the game has ended, this method gets called
    //prints out the winner or winners
    def gameEnd() = {
      val winners = g.winners
      val out = if (winners.length == 1) {
        s"The winner is ${winners(0).name}!"
      } else {
        s"The winners are ${winners.map(_.name).mkString(",")}"
      }
      updateLog(out)
      disableAfterGame() //game has ended so disable saving
    }

    //plays a single turn
    def playTurn() = {
      val turn = g.turn  //whose turn it is
      val player = g.getPlayers(turn) //the player in turn
      val in =  //input for which card will be played
        if (turn == 0) { //if turnIndex is 0 == it's a human player
          playerInput
        } else { //otherwise it's a bot and decides the card without extra info
          g.botPlayCard()
        }

      val card = player.returnHand(in)  //plays the card
      val choices = g.playCard(in) //get choices for what cards can be picked up
      val choice =  
        if (choices.flatten.isEmpty) { //if nothing can be picked up...
          Buffer()  //return an empty collection

        } else if (choices.size == 1) { //if there is only one choice..
          choices.head //return that choice

        } else { //if there is a choice that has to be made...
          val selection = if (turn == 0) { //ask the player which cards they want

            val text = s"choices:\n" + choices.map(_.map(_.toString).mkString(",")).mkString("\n") + s"\nChoose an ingeger in between 0 - ${choices.size - 1}"
            var ok = true
            var output: Option[String] = None
            do {
              output = Dialog.showInput(log, text, initial = "")
              val isInt = Try(output.get.toInt).isSuccess
              ok = if (isInt) (output.get.toInt <= choices.size - 1 && output.get.toInt >= 0) else false

            } while (!output.isDefined || !ok)
            output.get.toInt

          } else { //or allow the bot to select which cards they want
            player.decideSelection(choices)
          }

          //the cards are then removed from the board and added to the players pile
          g.pickupCards(choices(selection)) //the chosen cards are returned
        }

      //if the player picked something up, they're now the player who last picked something up
      if (!choice.isEmpty) g.changeLast

      val part3 = if (g.mokki) s"\n${player.name} clears the board!" else "" //check if the board was cleared
      g.changeTurn //change turn
      updateInfo  //update info about cards on the board/hand of the player
      //text for card pickup
      val part1 = player.name + " plays: " + card.toText
      val part2 = if (!choice.isEmpty) { ", gets: " + choice.map(_.toString).mkString(", ") } else ""
      updateLog(part1 + part2 + part3)  
    }

  }
}