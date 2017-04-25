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

object TextBasedGUI extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  def top = new MainFrame {

    val log = new TextArea(18, 80) {
      //minimumSize = preferredSize
      editable = false
      wordWrap = true
      lineWrap = true
      text = {
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
          "The first player to 16 points wins."
      }
    }
    val infoBox = new TextArea(3, 80) {
      editable = false
      wordWrap = true
      lineWrap = true
      text = "Start a new game from the menu bar!"
    }
    val input = new TextField(20) {
      minimumSize = preferredSize
    }
    this.listenTo(input.keys)

    def clearLog() = {
      this.log.text = ""
    }

    // Events: 
    this.reactions += {
      case keyEvent: KeyPressed =>
        if (keyEvent.source == this.input && keyEvent.key == Key.Enter && this.g.gameOn) {
          val command = this.input.text.trim
          if (command.nonEmpty) {
            if (command <= "9" && command >= "0") {
              val player = g.humanPlayer
              val in = command.toInt
              if (in < player.handSize && in >= 0) {
                processInput(in)
              }
            }
            this.input.text = ""
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

    def selection = new Frame {
      val cards = g.returnPlayers(0).returnHand
      val buttonToVal = new scala.collection.mutable.HashMap[AbstractButton, Int]()
      val valuePanel = new BoxPanel(Orientation.Horizontal) {
        for (ind <- cards.indices) {
          val v = ind
          val button = new Button(cards(ind).toString)
          button.border = new MatteBorder(1, 1, 1, 1, Color.gray)
          button.background = Color.blue
          button.preferredSize = new Dimension(60, 60)
          contents += button
          buttonToVal(button) = v
        }
      }
    }

    def popup = new Frame {
      val cards = g.returnCards
      title = "Select"
      val buttonToVal = new scala.collection.mutable.HashMap[AbstractButton, Int]()
      val valuePanel = new GridPanel(2, 2) {
        for (ind <- cards.indices) {
          val v = ind
          val button = new Button(cards(ind).toString)
          button.border = new MatteBorder(1, 1, 1, 1, Color.gray)
          button.background = Color.blue
          button.preferredSize = new Dimension(60, 60)
          contents += button
          buttonToVal(button) = v
        }
      }
      this.visible = true
      contents = new GridBagPanel() {
        val c = new Constraints
        c.fill = Fill.Horizontal
        c.gridx = 0
        c.gridy = 0
        layout(valuePanel) = c
        c.gridx = 0
        c.gridy = 1
      }
      reactions += {
        case ButtonClicked(b) => {
          playerInput = buttonToVal(b) - 1
          this.dispose()

        }
      }

    }

    def launchPopup(player: Player) = {
      val size = player.handSize
      var input: Option[String] = None
      var ok = false
      do {
        val text = if (size > 1) s"(0 - ${size - 1})" else (0)
        input = Dialog.showInput(log, s"Select card $text", initial = "")
        if (input.isDefined) {
          ok = (input.get >= "0" && input.get < size.toString)
        }
      } while (!input.isDefined || !ok)

      playerInput = input.get.toInt
    }

    // Set up the initial state of the GUI:

    this.title = "Kasino"
    this.location = new Point(700, 400)
    this.minimumSize = new Dimension(200, 200)
    this.pack()
    this.input.requestFocusInWindow()
    this.visible = true

    def processInput(in: Int) = {
      playerInput = in
      playTurn
      playUntilPlayer()
    }

    def updateLog(in: String) = {
      if (g.turn == 1) {
        clearLog
      }
      log.text += in + "\n"
    }

    def updateInfo = {
      this.infoBox.text = s"Cards on the table: ${g.returnCards.mkString(", ")}\nYour hand: ${g.returnPlayers(0).returnHand.mkString(", ")}"
    }

    def dealerInfo = {
      val dealer = g.returnPlayers(g.getDealer)
      updateLog(s"The current dealer is ${dealer.name}")
    }

    def startGame = {
      var opponents: Option[String] = None
      var ok = false
      val text = "How many opponents do you want to play against?\nChoose a number between 1 and 11.\nSuggested amounts are 2, 3 and 5."
      do {
        opponents = Dialog.showInput(log, text, initial = "")
        if (opponents.isDefined) {
          ok = (opponents.get >= "0" && opponents.get <= "9")
          if (ok) ok = (opponents.get.toInt <= 11 && opponents.get.toInt > 0)
        }
      } while (!opponents.isDefined || !ok)
      var name: Option[String] = None
      do {
        name = Dialog.showInput(log, "What's your name?", initial = "")
      } while (!name.isDefined)

      g = new Game(opponents.get.toInt, name.get, new Board, new Deck(0))
      saveAndQuit.enabled = (true)
      clearLog
      newWholeGame()
    }

    def loadGame = {
      g = ChunkIO.load
      if (g.gameOn) {
        clearLog
        loadedWholeGame
      } else {
        updateLog("The saved game was already over!")
      }
    }

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

    // Game:
    var g = new Game(5, "The Nameless One", new Board, new Deck(0))
    var playerInput = 0

    //main game loop that ends when atleast one player has 16 or more points
    def newWholeGame() = {
      updateLog("New game started!")
      newGame
      playUntilPlayer
    }

    def loadedWholeGame() = {
      updateLog("Game loaded!")
      scores
      playUntilPlayer()
    }

    //start a new game
    //sets up the deck, board and players so that a game can be played
    def newGame(): Unit = {
      updateLog("New round started!")
      dealerInfo
      g.newGame()
      updateInfo
    }

    def playUntilPlayer(): Unit = {
      if (g.gameOn && isRoundOver) newGame
      if (g.turn != 0) playBots()
      if (!g.gameOn) gameEnd
      if (g.gameOn && g.isRoundOver) {
        newGame
        playUntilPlayer()
      }
    }

    //plays turns until the player in turn is a human player
    def playBots() = {
      while (g.turn != 0 && !isRoundOver) {
        playTurn
      }
    }

    def isRoundOver() = {
      if (g.isRoundOver) {
        g.roundEnd()
        updateLog(s"Round over! \n${g.asName(g.last)} was last and clears the board.")
        scores

        true
      } else {
        false
      }
    }

    def gameRoundLoop() = {
      while (!g.isRoundOver) {
        playTurn()
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
      updateLog(out)
      saveAndQuit.enabled_=(false)
    }

    def scores = {
      val scores = g.getPlayerScores
      updateLog("Current scores: " + scores.mkString(" ") + "\n")
    }

    def playTurn() = {
      val turn = g.turn
      val player = g.returnPlayers(turn) //the player in turn
      val in =
        if (turn == 0) { //if turnIndex is 0 == it's a human player

          //          updateInfo
          //          launchPopup(player)
          playerInput

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

            val text = s"choices:\n" + choices.map(_.map(_.toString).mkString(",")).mkString("\n") + s"\nChoose an ingeger in between 0 - ${choices.size - 1}"
            var ok = true
            var output: Option[String] = None
            do {
              output = Dialog.showInput(log, text, initial = "")
              if (output.isDefined) {
                ok = (output.get >= "0" && output.get <= "9")
                if (ok) ok = (output.get.toInt <= choices.size - 1 && output.get.toInt >= 0)
              }
            } while (!output.isDefined || !ok)
            output.get.toInt

          } else { //or allow the bot to select
            player.decideSelection(choices)
          }

          //the cards are then removed from the board and added to the players pile
          g.pickupCards(choices(selection))
        }

      //if the player picked something up, they're the player who last picked something up
      if (!choice.isEmpty) g.changeLast

      val part3 = if (g.mokki) s"\n${player.name} clears the board!" else "" //check if the board was cleared
      g.changeTurn //change turn

      ChunkIO.saveGame(g)
      updateInfo
      //text for card pickup
      val part1 = player.name + " plays: " + card.toText
      val part2 = if (!choice.isEmpty) { ", gets: " + choice.map(_.toString).mkString(", ") } else ""
      updateLog(part1 + part2 + part3)
    }

  }
}