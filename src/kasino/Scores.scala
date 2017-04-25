package kasino
import collection.mutable.ArrayBuffer

class Scores (game: Game, count: Int) {
  private var scores: Array[Int] = Array.ofDim(count)
  
  //returns the scores of each player
  def ScoresWithIndex = this.scores.zipWithIndex.map( _.swap)
  
  //returns the indexes of players that won (had the same score)
  def getWinners: Array[Int] = {
    val topScore = scores.max
    scores.zipWithIndex.filter( _._1 == topScore).map(_._2)
  }
  
  //checks if the game is over, that is if someone has over 16 points
  def isGameOver = scores.max >= 16
  
  //adds a single point to a players score
  def addOne(index: Int) = {
    scores(index) += 1
  }
  
  //Collect all the cards from each players deck and add the points they gained to their total.
  def updatePoints() = {
    //val lastPickup = game.last/apparently lastPickup shouldn't give points like I'm used to when playing Kasino
    val players = game.getPlayers 
    
    //getPoints returns a tuple that has values calculated from the players pile like so:
    //(sum of points from point cards, amount of spades, amount of cards)
    val points = for (i <- players) yield i.getPoints //Get the points calculated from the decks of each player      
    var newPoints = points.map( _._1).toArray  //a list containing all the points for each player gained this round
    
    val topSpadeCount = points.map( _._2).max  //the highest amount of spades that a player has
    val topCardCount = points.map( _._3).max   //the highest amount of cards that a player has
    
    //all the players with the same highest amount of spades:
    val withMostSpades = points.map( _._2).zipWithIndex.filter( _._1 == topSpadeCount)
    
    //all the players with the same highest amount of cards:
    val withMostCards = points.map( _._3).zipWithIndex.filter( _._1 == topCardCount)
    
    //give points to the player(s) with most spades:
    if (withMostSpades.size == 1) {  //if 1 player has the most spades
      scores(withMostSpades.head._2) += 2  //give them 2 points
      
    } else if (withMostSpades.size == 2) { //if two players have the same, highest amount of points
      addOne(withMostSpades.head._2)  //give them both one point
      addOne(withMostSpades.last._2)  
    }  //if there are more, don't give anyone points
    
    //give points to the player(s) with most cards:
    if (withMostCards.size == 1) {  //if 1 player has the most cards
      addOne(withMostCards.last._2) //give them 1 point
    }  //if there are more, don't give anyone a point
    
    for (ind <- 0 until count) {  //add points from picked up cards
      scores(ind) += newPoints(ind) 
    }
  }
  
  //set the scores to those given in an array. Used when loading a game.
  def loadScores(in: Array[Int]) = {
    for (ind <- in.indices) {
      scores(ind) = in(ind)
    }
  }

}
object Scores {
  //create a new instance of scores class
  def apply(game: Game, n: Int) = new Scores(game, n)
}