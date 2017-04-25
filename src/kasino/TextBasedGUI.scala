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

    val log = new TextArea(14, 60) {
      editable = false
      wordWrap = true
      lineWrap = true
    }
    val infoBox = new TextArea(3, 60) {
      editable = false
      wordWrap = true
      lineWrap = true
    }
    val input = new TextField(20) {
      minimumSize = preferredSize
    }
    this.listenTo(input.keys)

    // Events: 

    this.reactions += {
      case keyEvent: KeyPressed =>
        if (keyEvent.source == this.input && keyEvent.key == Key.Enter && this.g.gameOn) {
          val command = this.input.text.trim
          if (command.nonEmpty) {
            this.input.text = ""
          }
          if (!g.gameOn) gameEnd
        }
    }

    //  Layout:

    this.contents = new GridBagPanel {
      import scala.swing.GridBagPanel.Anchor._
      layout += new Label("Location:") -> new Constraints(0, 0, 1, 1, 0, 1, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      //layout += new Label("Command:") -> new Constraints(0, 1, 1, 1, 0, 0, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      layout += new Label("Info:") -> new Constraints(0, 2, 1, 1, 0, 0, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      //      layout += turnCounter            -> new Constraints(0, 3, 2, 1, 0, 0, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      layout += log -> new Constraints(1, 0, 1, 1, 1, 1, NorthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
      //layout += input -> new Constraints(1, 1, 1, 1, 1, 0, NorthWest.id, Fill.None.id, new Insets(5, 5, 5, 5), 0, 0)
      layout += infoBox -> new Constraints(1, 2, 1, 1, 1, 1, SouthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
    }

    // Menu: 
    this.menuBar = new MenuBar {
      contents += new Menu("Program") {
        val quitAction = Action("Quit") { dispose() }
        val startAction = Action("New game") { startGame }
        val loadAction = Action("Load game") { loadGame }
        contents += new MenuItem(startAction)
        contents += new MenuItem(loadAction)
        contents += new MenuItem(quitAction)
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

    def updateLog(in: String) = {
      if (g.turn == 1) {
        log.text = ""
      }
      log.text += in + "\n"
    }

    def updateInfo = {
      this.infoBox.text = s"Cards on the table: ${g.returnCards.mkString(", ")}\nYour hand: ${g.returnPlayers(0).returnHand.mkString(", ")}"
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
      newWholeGame()
    }
    
    def loadGame = {
      g = ChunkIO.load
      if (g.gameOn) {
        loadedWholeGame()
      } else {
        updateLog("The saved game was already over!")
      }
    }

    // Game:
    var g = new Game(5, "The Nameless One", new Board, new Deck(0))
    var playerInput = 0

    //main game loop that ends when atleast one player has 16 or more points
    def newWholeGame() = {
      updateLog("New game started!")
      while (g.gameOn) {
        newGame
        g.changeTurn
        scores
      }
      gameEnd
    }

    //start a new game (a game is a single deck played through)
    def newGame() = {
      updateLog("New round started!")
      g.setup()
      g.dealCards()
      gameRoundLoop
    }

    def loadedWholeGame() = {
      updateLog("Game loaded!")
      scores
      gameRoundLoop
      g.changeTurn
      scores

      while (g.gameOn) {
        newGame
        g.changeTurn
        scores
      }
      gameEnd
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

          updateInfo
          launchPopup(player)
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
      //text for card pickup
      val part1 = player.name + " plays: " + card.toText
      val part2 = if (!choice.isEmpty) { ", gets: " + choice.map(_.toString).mkString(", ") } else ""
      updateLog(part1 + part2 + part3)
    }

  }
}