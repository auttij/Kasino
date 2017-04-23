package kasino
import collection.mutable.ArrayBuffer

class Scores (game: Game, count: Int) {
  private var scores: Array[Int] = Array.ofDim(count)
  
  def scoresWithIndex = scores.zipWithIndex.map( _.swap)
  
  //returns the indexes of players that won (had the same score)
  def getWinners: Array[Int] = {
    val topScore = scores.max
    scores.zipWithIndex.filter( _._1 == topScore).map(_._2)
  }
  
  def isGameOver = scores.max >= 16
  
  //adds a single point to a players score
  def addOne(index: Int) = {
    scores(index) += 1
  }
  
  def updatePoints() = {
    val lastPickup = game.returnLastPickup //apparently lastPickup shouldn't give points like I'm used to...
    val players = game.returnPlayers
    val points = for (i <- players) yield i.getPoints             
    var newPoints = points.map( _._1).toArray  //a list containing all the points for each player gained this round
    
    val mostSpades = points.map(_._2).zipWithIndex.maxBy(_._1)._2 //the index of the player with most Spades
    val mostCards = points.map(_._3).zipWithIndex.maxBy(_._1)._2  //the index of the player with most Cards
    //create an array of players that get points from last pickup, most cards and spades
    val addPointsTo: Array[Int] = Array(mostCards, mostSpades, mostSpades) 
    addPointsTo.foreach(addOne)  //add one point to each of them
    
    for (ind <- 0 until count) {  //add points from other cards
      scores(ind) += newPoints(ind) 
    }
  }

}
object Scores {
  def apply(game: Game, n: Int) = new Scores(game, n)
}