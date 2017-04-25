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
  private val fileName = "saveFile.txt" //name of the file the game will be saved to

  //Decrypts the save file and gives it to the reader
  def load: Game = {
    try {
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

      //create new instances of deck, board, and Game
      val opponentCount = playerCount - 1
      val deck = new Deck(0)
      val board = new Board
      val game = new Game(opponentCount, playerName, board, deck)

      //if the game is over, it shouldn't be loaded and thus an exception is thrown to prevent errors
      if (gameOver == "1") {
        throw new CorruptedSaveFileException("The game has already ended.")
      }

      //get the card data from deckChunk and add cards to the deck.
      if (deckChunk != "") {
        val deckCards = for (card <- deckChunk.split("/")) yield cardMatch(card)
        deck.addCards(deckCards)
      }

      //get the card data from boardChunk and add cards to the Board.
      if (boardChunk != "") {
        val boardCards = for (card <- boardChunk.split("/")) yield cardMatch(card)
        board.addCards(boardCards)
      }

      //create multiple files of data from handChunk that represent player hands
      val handChunks = for (hand <- handChunk.split("#")) yield hand
      //create multiple files of data from pileChunk that represent player piles
      val pileChunks = for (pile <- pileChunk.split("#")) yield pile

      var handIndex = 0 //keep track of the player who's data is being accessed
      for (hand <- handChunks) { //go through all the hands in handsChunks
        if (!hand.isEmpty()) { //if the hand data is not empty, add cards in the chunk to the players hand
          val cards = for (card <- hand.split("/")) yield cardMatch(card)
          game.getPlayers(handIndex).addCards(cards.toBuffer)
        }
        handIndex += 1
      }

      var pileIndex = 0 //keep track of the player who's data is being accessed
      for (pile <- pileChunks) { //go through all the piles in pileChunks
        if (!pile.isEmpty()) { //if the pile data is not empty, add cards in the chunk to the players pile
          val cards = for (card <- pile.split("/")) yield cardMatch(card)
          game.getPlayers(pileIndex).addToPile(cards.toBuffer)
        }
        pileIndex += 1
      }

      //if the amount of players, and amount of hands filled don't match, throw an exception.
      if (playerCount != handIndex) {
        throw new CorruptedSaveFileException("Not enough hands in save data")
      }

      //load the indexes for who's in turn, last pickup and dealer.
      game.loadTurnLastDealer(turn, lastPickup, dealer)

      //load the scores
      val scores = for (score <- scoreChunk.split("/")) yield score.toInt
      game.loadScores(scores)

      //finally, return the succesfully loaded game
      game
    } catch { //catch exceptions and throw a new CorruptedSaveFileException
      case e: IOException =>
        val saveExc = new CorruptedSaveFileException("Reading the save data failed.")
        saveExc.initCause(e)
        throw saveExc

      case e: NumberFormatException => { //if a string, that can't be converted toInt is converted toInt
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

    //get all the information that needs to be stored and save it as strings.
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

    //the names of all the different chunks and what they contain.
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

    //create a single string of data that will be saved
    val saveData = {
      val out =
        for (part <- chunks) yield part._1 + len(part._2) + part._2
      "KASINO" ++ out
    }

    //a printWriter that writes the data to the file
    val file = new PrintWriter(fileName)

    try {
      //write the data to the file, encrypted with Base 64 encoding
      file.print(Helpers.B64encode(saveData.mkString))

    } finally {
      file.close
    }
  }

  //a function with a short name for calling the helper function that creates strings from int's with 3 character length
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

  //The read-method of the Reader class will occasionally read only part of
  //the characters that were requested. This method will repeatedly call read
  //to completely fill the given buffer. The size of the buffer tells the
  //algorithm how many bytes should be read.
  def readFully(result: Array[Char], input: Reader) = {
    var cursor = 0
    while (cursor != result.length) {
      var numCharactersRead = input.read(result, cursor, result.length - cursor)

      if (numCharactersRead == -1) { // If the file end is reached before the buffer is filled an exception is thrown.
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