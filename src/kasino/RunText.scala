package kasino

object RunText extends App {
  val opponentCount = 5
  val playerName = "falarikae"
  
  val kasino = new KasinoText(opponentCount, playerName)
  kasino.newWholeGame()
  }