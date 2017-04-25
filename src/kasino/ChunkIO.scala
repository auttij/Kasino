package kasino

import scala.io.Source
import java.io.Reader
import java.util.Base64
import java.nio.charset.StandardCharsets
import java.io.PrintWriter
import scala.collection.mutable.Buffer
import java.io.IOException
import java.io.StringReader

object ChunkIO {

  //Decrypts the save file and gives it to the reader
  def load: Game = {
    try {
      val fileName = "saveFile.txt"
      val input = for (line <- Source.fromFile(fileName).getLines()) yield line
      val decoded = Helpers.B64decode(input.mkString)
      val reader = new StringReader(decoded)
      loadGame(reader)
    } catch {
      case e: java.lang.IllegalArgumentException =>
        val saveExc = new CorruptedSaveFileException("Corrupted Save data, couldn't be decrypted.")
        saveExc.initCause(e)
        throw saveExc
    }
  }

  //processes save data and creates a new game
  def loadGame(input: Reader): Game = {
    //a helper method. Given the header of an array, it reads for as longs as instructed and returns the read data.
    def handleChunk(chunkHeader: Array[Char]) = {
      val arr = new Array[Char](Helpers.extractChunkSize(chunkHeader))
      Helpers.readFully(arr, input)
      arr
    }

    //takes a textual description of a card and then creates that card
    def cardMatch(input: String): Card = {
      def suit(in: Char) = {
        in match {
          case in if (in == 'S') => Spades
          case in if (in == 'D') => Diamonds
          case in if (in == 'C') => Clubs
          case in if (in == 'H') => Hearts
          case _                 => throw new CorruptedSaveFileException("Unknown card suit.")
        }
      }

      new Card(input.drop(1).toInt, suit(input(0)))
    }

    var header = new Array[Char](6)
    var chunkHeader = new Array[Char](6)

    try {
      Helpers.readFully(header, input)

      if (!header.mkString.startsWith("KASINO")) {
        throw new CorruptedSaveFileException("Unknown file type")
      }

      //variables where values read from data are stored
      var gameOver = ""
      var playerCount = 0
      var playerName = ""
      var turn = 0
      var lastPickup = 0
      var dealer = 0
      var endFound = false
      var deckChunk = ""
      var boardChunk = ""
      var scoreChunk = ""
      var handChunk = ""
      var pileChunk = ""

      //read data until "END" is found
      while (!endFound) {
        Helpers.readFully(chunkHeader, input)
        Helpers.extractChunkName(chunkHeader) match {
          case "OVR" => gameOver = handleChunk(chunkHeader).mkString
          case "OPC" => playerCount = handleChunk(chunkHeader).mkString.toInt
          case "NAM" => playerName = handleChunk(chunkHeader).mkString
          case "TRN" => turn = handleChunk(chunkHeader).mkString.toInt
          case "DLR" => dealer = handleChunk(chunkHeader).mkString.toInt
          case "LST" => lastPickup = handleChunk(chunkHeader).mkString.toInt
          case "DCK" => deckChunk = handleChunk(chunkHeader).mkString
          case "BRD" => boardChunk = handleChunk(chunkHeader).mkString
          case "SCR" => scoreChunk = handleChunk(chunkHeader).mkString
          case "HND" => handChunk = handleChunk(chunkHeader).mkString
          case "PLS" => pileChunk = handleChunk(chunkHeader).mkString
          case "END" => endFound = true
          case _     => var unknown = handleChunk(chunkHeader)
        }
      }

      val opponentCount = playerCount - 1
      val deck = new Deck(0)
      val board = new Board
      val game = new Game(opponentCount, playerName, board, deck)

      if (gameOver == "1") {
        throw new CorruptedSaveFileException("The game has already ended.")
      }

      if (deckChunk != "") {
        val deckCards = for (card <- deckChunk.split("/")) yield cardMatch(card)
        deck.addCards(deckCards)
      }

      if (boardChunk != "") {
        val boardCards = for (card <- boardChunk.split("/")) yield cardMatch(card)
        board.addCards(boardCards)
      }

      val scores = for (score <- scoreChunk.split("/")) yield score.toInt
      val handChunks = for (hand <- handChunk.split("#")) yield hand
      val pileChunks = for (pile <- pileChunk.split("#")) yield pile

      var handIndex = 0
      for (hand <- handChunks) {
        if (!hand.isEmpty()) {
          val cards = for (card <- hand.split("/")) yield cardMatch(card)
          game.getPlayers(handIndex).addCards(cards.toBuffer)
        }
        handIndex += 1
      }

      var pileIndex = 0
      for (pile <- pileChunks) {
        if (!pile.isEmpty()) {
          val cards = for (card <- pile.split("/")) yield cardMatch(card)
          game.getPlayers(pileIndex).addToPile(cards.toBuffer)
        }
        pileIndex += 1
      }

      if (playerCount != handIndex) {
        throw new CorruptedSaveFileException("Not enough hands in save data")
      }

      game.loadTurnLastDealer(turn, lastPickup, dealer)
      game.loadScores(scores)

      game
    } catch {
      case e: IOException =>
        val saveExc = new CorruptedSaveFileException("Reading the save data failed.")
        saveExc.initCause(e)
        throw saveExc
        
      case e: NumberFormatException => {
        val saveExc = new CorruptedSaveFileException("There was data that couldn't be converted to an integer when it should have been.")
        saveExc.initCause(e)
        throw saveExc
      }
    }
  }

  //data that is saved: opponent count, player name, remaining cards in deck, remaining cards on board,
  //scores for each player, hands of each player, card piles of each player, who's turn it is and who was last pickup
  def saveGame(game: Game) = {
    val players = game.getPlayers
    val deck = game.deck
    val board = game.board

    val opponentCount = players.size.toString
    val playerName = game.playerName
    val turn = game.turn.toString
    val lastPickup = game.last.toString
    val deckCards = deck.returnCards.mkString("/")
    val gameOver = if (!game.gameOn) "1" else "0"
    val dealer = game.getDealer.toString
    val boardCards = board.returnCards.mkString("/")
    val scores = game.getPlayerScores.map(x => x._2).mkString("/")
    val playerHands = { for (player <- players) yield player.returnHand }.map(_.map(_.toString()).mkString("/")).mkString("#")
    val playerPiles = { for (player <- players) yield player.returnPile.mkString("/") }.mkString("#")

    val chunks: Array[(String, String)] = Array(
      ("OVR", gameOver),
      ("OPC", opponentCount),
      ("NAM", playerName),
      ("TRN", turn),
      ("DLR", dealer),
      ("LST", lastPickup),
      ("DCK", deckCards),
      ("BRD", boardCards),
      ("SCR", scores),
      ("HND", playerHands),
      ("PLS", playerPiles),
      ("END", "000"))

    val saveData = {
      val out =
        for (part <- chunks) yield part._1 + len(part._2) + part._2
      "KASINO" ++ out
    }

    val fileName = "saveFile.txt"
    val file = new PrintWriter(fileName)

    try {
      file.print(Helpers.B64encode(saveData.mkString))

    } finally {
      file.close
    }
  }

  private def len(input: String): String = Helpers.ThreeLenStr(input.length())

}

object Helpers {
  //given a chunk header (an array of 5 chars) will return the size of this chunks data
  def extractChunkSize(chunkHeader: Array[Char]): Int = {
    100 * (chunkHeader(3) - '0') + 10 * (chunkHeader(4) - '0') + (chunkHeader(5) - '0')
  }

  //given a chunk header (an array of 5 chars) will return the name of this chunks data
  def extractChunkName(chunkHeader: Array[Char]): String = {
    chunkHeader.take(3).mkString
  }

  def readFully(result: Array[Char], input: Reader) = {
    var cursor = 0
    while (cursor != result.length) {
      var numCharactersRead = input.read(result, cursor, result.length - cursor)

      if (numCharactersRead == -1) {
        throw new CorruptedSaveFileException("Unexpected end of file.")
      }
      cursor += numCharactersRead
    }
  }

  //Encodes a string with Base 64 encryption
  def B64encode(input: String): String =
    Base64.getEncoder.encodeToString(input.getBytes(StandardCharsets.UTF_8))

  //decodes a string that has been encoded with Base 64 encryption
  def B64decode(input: String) = {
    val bytes = Base64.getDecoder.decode(input.getBytes(StandardCharsets.UTF_8))
    new String(bytes, StandardCharsets.UTF_8)
  }

  //takes a 1 - 3 digit integer and returns it as a 3 character long string
  //for example 7 -> "007"
  def ThreeLenStr(input: Int): String = f"$input%03d"

}

class CorruptedSaveFileException(message: String) extends Exception(message)