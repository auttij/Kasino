package kasino

object RunText extends App {

  val text = "how many players do you want to play against? (1-11)\nSuggested amounts are 2, 3, 5 or 11\n"
  val text2 = "what is your name?\n"
  var ok = true
  var count = 3
  do {
    val out = readLine(text)
    ok = (!out.isEmpty) && (out <= "9") && (out >= "1")
    if (ok) ok = (out.toInt <= 11)
    if (ok) count = out.toInt
  } while (!ok)

  val opponentCount = count
  val playerName = readLine(text2)

  val kasino = new KasinoText(opponentCount, playerName)
  kasino.newWholeGame()
}