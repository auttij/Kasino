package kasino

import scala.collection.mutable.Buffer

//represents a single game of Kasino
class Game(opponents: Int, val playerName: String, val board: Board, val deck: Deck) {
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i)) //the human player and bots
  private val scores = Scores(this, players.size) //keeps track of the scores for this game
  private var gameOver = false //has the game ended?
  private var turnIndex = 1 //stepper, keeps track of the player in turn
  private var lastPickup = 0 //most recent keeper
  private var dealer = 0 //stepper, keeps track of who the dealer is

  def turn = this.turnIndex //returns the current turn
  def last = this.lastPickup //returns the index of the player who last picked up a card
  def getDealer = this.dealer //returns the index of the player who is the dealer
  def getPlayers = this.players //return all players
  def gameOn = !this.gameOver //check if game is still ongoing
  def getCards = this.board.returnCards //tells the cards on the table so the can be displayed in GUI
  def humanPlayer = this.players(0) //quickly return the human player  

  //change the turnIndex, lastPickup and Dealer, used when loading.
  def loadTurnLastDealer(turn: Int, last: Int, dealer: Int) = {
    this.turnIndex = turn
    this.lastPickup = last
    this.dealer = dealer
  }

  //sets up a new game:
  //fills the deck and deals cards to players
  def newGame() = {
    setup
    dealCards
  }

  //sets the board and deck in a playable state for a new game
  def setup() = {
    deck.initialize
    deck.shuffle
    board.addCards(deck.deal(4))
  }

  //deals 4 cards to all players, used in the beginning of a game.
  def dealCards() = {
    for (i <- 0 until players.size) {
      players(i).addCards(deck.deal(4))
    }
  }
  
  //adds a single card to a players hand, if the deck is not empty
  def addSingleCard(player: Player) = {
    if (!this.deck.isEmpty) player.addCards(deck.deal(1))
  }

  //plays the given card from players hand to the Board.
  //returns a Buffer[Buffer[Card]] containing the choices of cards that can be picked up.
  def playCard(in: Int): Buffer[Buffer[Card]] = {
    val player = players(turn)
    val card = player.playCard(in) //the card that will be played
    addSingleCard(player)  //once the player has played a card, give them a new one if possible
    val choices = board.playCard(card) //creates choices of what can be picked up

    if (choices.isEmpty) { //if nothing can be picked up...
      board.addCards(Seq(card)) //play the card to the table
      Buffer(Buffer[Card]())

    } else if (choices.size == 1) { //if there is only one choice..
      val cards = board.removeCards(choices(0)) //the card that can be picked up are automatically picked up..
      player.addToPile(cards) //and added to the players pile
      Buffer(cards)

    } else { //if there is a choice that has to be made...
      choices
    }

  }

  //given an index, returns the name of the player with that index
  def asName(index: Int) = {
    players(index).name
  }

  //returns all players with their scores.
  def getPlayerScores = {
    scores.getScores.map(x => (asName(x), x))
  }

  //picks up the given cards from the Board and add them to a players pile.
  def pickupCards(in: Buffer[Card]) = {
    val player = players(turn)
    val cards = board.removeCards(in)
    player.addToPile(cards)
    cards
  }

  //given an array of scores, changes the games scores to those given in the array
  def loadScores(in: Array[Int]) = {
    scores.loadScores(in)
  }

  //get a card selection from the bot player in turn
  def botPlayCard() = players(turn).decideCard

  //changes the player who is currently playing to the next player
  def changeTurn() = turnIndex = (turn + 1) % players.length

  //changes the player who is the dealer for the next round and
  //set the player in turn to be the player after the dealer.
  def changeDealer() = {
    dealer = (dealer + 1) % players.length
    turnIndex = (dealer + 1) % players.length
  }

  //did the game end? if so gameOver = true
  def didGameEnd() = if (scores.isGameOver) this.gameOver = true

  //the winners of the game
  def winners = scores.getWinners.map(x => players(x))

  //checks if the round is over, that is if no player has cards in their hand
  def isRoundOver = {
    val handSizes = for (player <- players) yield player.handSize
    handSizes.max == 0
  }

  //when a round ends (all cards have been played) collect remaining cards and update points
  def roundEnd() = {
    players(last).addToPile(board.collectAll)  //give all cards from the board to the player who last picked up a card
    scores.updatePoints
    changeDealer
    didGameEnd
  }

  //checks if the board has been cleared and if so, gives the player who cleared it a point
  def mokki(): Boolean = {
    if (board.returnCards.isEmpty) {
      scores.addOne(turn)
      true
    } else {
      false
    }
  }

  def changeLast = this.lastPickup = turnIndex

}