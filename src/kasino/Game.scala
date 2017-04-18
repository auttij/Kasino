package kasino

class Game(opponents: Int, playerName: String) {
  private val deck = new Deck(0)
  private val board = Board
  private val players: Seq[Player] = Seq(new HumanPlayer(playerName)) ++ (for (i <- 0 until opponents) yield Bot(i))
  private val scores = Scores(this, players.size)
  private val rounds = 48 / (players.length * 4) //tells the amount of rounds that will be played
  private var gameOver = false
  private var turnIndex = 0 //stepper
  private var lastPickup = 0

  def turn = this.turnIndex
  def returnPlayers = this.players
  def returnLastPickup = this.lastPickup
  def gameOn = !this.gameOver  //check if game is still ongoing
  def playerCount = this.players.size
  def roundCount = this.rounds
  def returnCards = this.board.cards
  
  //deals 4 cards to all players
  def dealCards() = {
    for (i <- 0 until playerCount) {
        players(i).addCards(deck.deal(4))
    }
  }
  
  def playCard() = players(turnIndex).decideCard
  
  //changes the player who is currently playing to the next player
  def changeTurn() = turnIndex = (turnIndex + 1) % players.length

  //did the game end? if so gameOver = true
  def didGameEnd() = if (scores.isGameOver) gameOver = true
  
  //the winners of the game
  def winners = scores.getWinners.map(x => players(x))
  
  def setup() = {
    deck.initialize
    deck.shuffle
    board.addCards(deck.deal(4))
  }
  
  //when a round ends (deck is empty) collect remaining cards and update points
  def roundEnd() = {
    players(lastPickup).addToPile(Board.collectAll)
    players(lastPickup).addToPile(deck.collectAll)
    scores.updatePoints
    didGameEnd
  }
  
  //checks if the board has been cleared and if so, gives the player who cleared it a point
  def mokki() = {
    if (Board.cards.isEmpty) {
      scores.addOne(turnIndex)
    }
  }
  
  def changeLast = this.lastPickup = turnIndex

}