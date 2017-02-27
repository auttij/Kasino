package kasino

abstract class Suit (val suit: Char) {
  def toString: String
  
}

case object Spades extends Suit('S') {
  override def toString = " of Spades"
}

case object Diamonds extends Suit('D') {
  override def toString = " of Diamonds"
}

case object Clubs extends Suit('C') {
  override def toString = " of Clubs"
}

case object Hearts extends Suit('H') {
  override def toString = " of Hearts"
}



class Card (value: Int, suit: Suit) {
    
  override def toString = suit.suit + value.toString
  
  def toText = value match {
    case 1 => "Ace" + suit
    case 2 => "Two" + suit
    case 3 => "Three" + suit
    case 4 => "Four" + suit
    case 5 => "Five" + suit
    case 6 => "Six" + suit
    case 7 => "Seven" + suit
    case 8 => "Eight" + suit
    case 9 => "Nine" + suit
    case 10 => "Ten" + suit
    case 11 => "Jack" + suit
    case 12 => "Queen" + suit
    case 13 => "King" + suit
    case _ => "illegal value"
  }
  
  def tableValue = value
  
  def handValue = value match {
    case 2 if (suit == Spades) => 15
    case 10 if (suit == Diamonds) => 16
    case 1 => 14
    case _ => value
  }
  
  def pointValue = value match {
    case 2 if (suit == Spades) => 1
    case 10 if (suit == Diamonds) => 2
    case 1 => 1
    case _ => 0
  }
}