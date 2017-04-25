package kasino

import scala.collection.mutable.Buffer

class Game(opponents: Int, val playerName: String, val board: Board, val deck: Deck) {
//  private val deck = new Deck(0)
//  private val board = Board
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private val scores = Scores(this, players.size)
  private val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played
  private var gameOver = false
  private var turnIndex = 1   //stepper
  private var lastPickup = 0  //most recent keeper
  private var dealer = 0
  
  def turn = this.turnIndex
  def getDealer = this.dealer
  def returnPlayers = this.players
  def last = this.lastPickup
  def gameOn = !this.gameOver //check if game is still ongoing
  def playerCount = this.players.size
  def roundCount = this.rounds
  def returnCards = this.board.cards
  def humanPlayer = this.players(0)
  
  //deals 4 cards to all players
  def dealCards() = {
    for (i <- 0 until playerCount) {
      players(i).addCards(deck.deal(4))
    }
  }
  
  def loadTurnLastDealer(turn: Int, last: Int, dealer: Int) = {
    this.turnIndex = turn
    this.lastPickup = last
    this.dealer = dealer
  }
  
// def playerCard(in: Int) = players(turn).returnHand(in)
  
  //plays the given card from players hand to a Board.
  //if it doesn't pick up anything it gives an empty buffer that can be processed
  //if it gives 1 choice that can be picked up, it picks it up
  //if it gives multiple choices, it doesn't pick it up and returns the choice
  def playCard(in: Int): Buffer[Buffer[Card]] = {
    val player = players(turnIndex)
    val card = player.playCard(in) //the card that will be played
    addSingleCard(player)
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
  
  //adds a single card to a players hand, if the deck is not empty
  def addSingleCard(player: Player) = {
    if (!this.deck.isEmpty) player.addCards(deck.deal(1))
  }

  def getPlayerScores = {
    scores.scoresWithIndex.map( x => (players(x._1).name, x._2) )
  }
  
  def asName(index: Int) = {
    players(index).name
  }
  
  //picks up the given cards from the Board
  def pickupCards(in: Buffer[Card]) = {
    val player = players(turnIndex)
    val cards = board.removeCards(in)
    player.addToPile(cards)
    cards
  }
  
  def loadScores(in: Array[Int]) = {
    scores.loadScores(in)
  }
  
  //get a card selection from the bot player in turn
  def botPlayCard() = players(turnIndex).decideCard

  //changes the player who is currently playing to the next player
  def changeTurn() = turnIndex = (turnIndex + 1) % players.length
  
  //changes the player who is the dealer for the next round and
  //set the player in turn to be the player after the dealer.
  def changeDealer() = {
    dealer = (dealer + 1) % players.length
    turnIndex = (dealer + 1) % players.length
  }
  
  //did the game end? if so gameOver = true
  def didGameEnd() = if (scores.isGameOver) endGame

  //forces the game to end
  private def endGame() = this.gameOver = true
  
  //the winners of the game
  def winners = scores.getWinners.map(x => players(x))

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

  def isRoundOver = {
    val handSizes = for (player <- players) yield player.handSize
    handSizes.max == 0
  }
  
  //when a round ends (all cards have been played) collect remaining cards and update points
  def roundEnd() = {
    players(lastPickup).addToPile(board.collectAll)
    scores.updatePoints
    changeDealer
    didGameEnd
    ChunkIO.saveGame(this)
  }

  //checks if the board has been cleared and if so, gives the player who cleared it a point
  def mokki(): Boolean = {
    if (board.cards.isEmpty) {
      scores.addOne(turnIndex)
      true
    } else {
      false
    }
  }

  def changeLast = this.lastPickup = turnIndex

}