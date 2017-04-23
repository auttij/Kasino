package kasino

//The class Suit represents a suit of a playing card
//There are exactly four instances of this class: `Spades`, `Diamonds`, `Clubs` and `Hearts`.
//All the Suit objects are immutable.

//Each object has a toString method that returns a textual representation of the Suits name
abstract class Suit(val suit: Char) {
  def toString: String
}

//The following objects represents each of the suits in a deck of cards.
//This immutable singleton object represents the suit Spades. It is one of the four predefined instances of class `Suit`.
case object Spades extends Suit('S') { override def toString = "Spades" }

//This immutable singleton object represents the suit Diamonds. It is one of the four predefined instances of class `Suit`.
case object Diamonds extends Suit('D') { override def toString = "Diamonds" }

//This immutable singleton object represents the suit Clubs. It is one of the four predefined instances of class `Suit`.
case object Clubs extends Suit('C') { override def toString = "Clubs" }

//This immutable singleton object represents the suit Hearts. It is one of the four predefined instances of class `Suit`.
case object Hearts extends Suit('H') { override def toString = "Hearts" }

//The class Card represent a single playing card in a deck of cards.
//A card has two values attached to it, a value representing it's face value 
//and a suit representing it's suit.
class Card(value: Int, val suit: Suit) {
  if (value < 1 || value > 13) {
    throw new IllegalArgumentException("Illegal card value.")
  }

  override def toString = suit.suit + value.toString

  //returns a textual representation of the card
  //for example "Ace of Spades", "Queen of Hearts", "Two of Diamonds"
  def toText: String = {
    val t = value match {
      case 1  => "Ace"
      case 2  => "Two"
      case 3  => "Three"
      case 4  => "Four"
      case 5  => "Five"
      case 6  => "Six"
      case 7  => "Seven"
      case 8  => "Eight"
      case 9  => "Nine"
      case 10 => "Ten"
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
    }

    t + " of " + suit
  }

  //returns the value of the card as integer, as it is on the table in a game of Casino. 
  def tableValue: Int = value

  //returns the value of the card as an integer, when it's in the players hand
  //for most of the cards this is their face value but there are a few exceptions
  def handValue: Int = value match {
    case 2 if (suit == Spades)    => 15 //if the card is the Two of Spades, handValue is 15
    case 10 if (suit == Diamonds) => 16 //if the card is the Ten of Diamonds, handValue is 16
    case 1                        => 14 //if the card is any Ace, handValue is 14
    case _                        => value //for all other cards their handValue is their face value
  }

  //returns the value of the card as an integer, that represents it's value in the game's scoring system  
  def pointValue: Int = value match {
    case 2 if (suit == Spades)    => 1 //if the card is the Two of Spades, it's worth 1 point
    case 10 if (suit == Diamonds) => 2 //if the card is the Ten of Diamonds, it's worth 2 points
    case 1                        => 1 //if the card is an Ace, it's worth 1 point
    case _                        => 0 //other cards aren't worth points
  }
}