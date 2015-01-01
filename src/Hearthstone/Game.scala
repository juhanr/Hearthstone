package Hearthstone

import collection.mutable._
import util.Random.shuffle

object Game extends App {
  val redDeckPath = "res/deck_red.txt"
  val blueDeckPath = "res/deck_blue.txt"
  val inputPrefix = "> "
  val menu =
    "\nPlease choose an action:\n" +
      "1. View field\n" +
      "2. View hand\n" +
      "3. View card info\n" +
      "4. Play a card\n" +
      "5. Attack with a card\n" +
      "6. End turn\n"
  var red: Player = null
  var blue: Player = null

  newGame

  def newGame: Unit = {
    red = new Player("Red", Parser.parseDeck(Parser.readFile(redDeckPath)), 30, 1)
    blue = new Player("Blue", Parser.parseDeck(Parser.readFile(blueDeckPath)), 30, 1)
    shuffleDeck(red.deck)
    shuffleDeck(blue.deck)
    println("===== Hearthstone =====")
    startTurn(red, blue)
  }

  def startTurn(player: Player, opponent: Player): Unit = {
    println("\n--- " + player.name + " player's turn ---")
    val drawnCard = player.deck.cards.head
    println("Drawn card: " + drawnCard.name)
    player.hand += drawnCard
    player.printStats

    while (parseInput(player, readLine(menu+inputPrefix))) {}
  }

  def parseInput(player: Player, input: String): Boolean = input.trim.head match {
    case '1' =>
      red.printField; blue.printField; return true
    case '2' =>
      player.printHand; return true
    case '3' =>
      cardAction("View"); return true
    case '4' =>
      cardAction("Play"); return true
    case '5' =>
      cardAction("Attack"); return true
    case '6' =>
      println("Turn ended."); return false
    case _ => println("Invalid input!"); return true
  }

  def cardAction(action: String): Unit = {
    println("Please enter the card name.")
    val cardName = readLine(inputPrefix)
  }

  def shuffleDeck(deck: Deck): Unit = {
    deck.cards = shuffle(deck.cards)
  }
}