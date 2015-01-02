package Hearthstone

import collection.mutable.ListBuffer
import util.Random.shuffle

object Game extends App {
  val redDeckPath = "res/deck_red.txt"
  val blueDeckPath = "res/deck_blue.txt"
  val redPlayerName = "Red player"
  val bluePlayerName = "Blue player"
  val inputPrefix = "> "
  val menu =
    "\nPlease choose an action:\n" +
      "1. View field\n" +
      "2. View hand\n" +
      "3. View card info\n" +
      "4. Play a card from your hand\n" +
      "5. Attack with a card\n" +
      "6. End turn\n"
  val enterName = "Please enter the card name. (Type nothing to go back)\n"
  val noCards = "You don't have any cards left in your deck. You lost 10 HP!"

  var red: Player = null
  var blue: Player = null
  var allCards: ListBuffer[Card] = null
  var currentPlayer: Player = null
  var otherPlayer: Player = null
  var isGameOver = false

  newGame

  /*
   * Starts a new game, assigns default variables.
   */
  def newGame: Unit = {
    isGameOver = false
    red = new Player(redPlayerName, Parser.parseDeck(Parser.readFile(redDeckPath)), 30, 0)
    blue = new Player(bluePlayerName, Parser.parseDeck(Parser.readFile(blueDeckPath)), 30, 0)
    allCards = red.deck.cards ++ blue.deck.cards
    shuffleDeck(red.deck)
    shuffleDeck(blue.deck)
    currentPlayer = blue
    println("===== Hearthstone =====")
    nextTurn
  }

  /*
   * Starts the other player's turn.
   */
  def nextTurn: Unit = {
    if (currentPlayer == red) {
      currentPlayer = blue
      otherPlayer = red
    } else {
      currentPlayer = red
      otherPlayer = blue
    }
    currentPlayer.field.foreach(_.canAttack = true)
    if (currentPlayer.AP < 10) currentPlayer.AP += 1
    println("\n--- " + currentPlayer.name + "'s turn ---")
    if (currentPlayer.deck.cards.length > 0) drawCard
    else {
      println(noCards)
      currentPlayer.health -= 10
      if (currentPlayer.health <= 0) {
        gameOver(otherPlayer)
        return
      }
    }
    currentPlayer.printStats

    while (parseInput(readLine(menu + inputPrefix)) && !isGameOver) {}
    if (!isGameOver) nextTurn
  }

  /*
   * Draws a card from the current player's deck.
   */
  def drawCard: Unit = {
    val drawnCard = currentPlayer.deck.cards.head
    currentPlayer.deck.cards -= drawnCard
    println("Drawn card: " + drawnCard.name)
    currentPlayer.hand += drawnCard
  }

  /*
   * Parses the given menu input.
   * Returns whether an action should be prompted again or not.
   */
  def parseInput(input: String): Boolean = input.trim.head match {
    case '1' =>
      red.printField; blue.printField; return true
    case '2' =>
      currentPlayer.printHand; return true
    case '3' =>
      viewCard; return true
    case '4' => {
      if (currentPlayer.hand.length > 0) playCard(chooseCard(currentPlayer.hand, "Please choose a card: "))
      else println("There are no cards in your hand!")
      return true
    }
    case '5' => {
      if (currentPlayer.field.length > 0) attackWithCard(chooseCard(currentPlayer.field, "Please choose a card: "))
      else println("There are no cards in your side of the field!")
      return true
    }
    case '6' =>
      println("Turn ended."); return false
    case _ => println("Invalid input!"); return true
  }

  /*
   * Asks the player for a card name and displays its full information.
   */
  def viewCard: Unit = {
    var cardName = ""
    while (true) {
      cardName = readLine(enterName + inputPrefix).trim
      if (cardName == "") return
      for (card <- allCards) if (card.name == cardName) { println(card); return }
      println("Card not found!")
    }
  }

  /*
   * Displays a given list of cards, asks the player to choose one 
   * and returns the chosen card.
   */
  def chooseCard(cards: ListBuffer[Card], msg: String): Card = {
    println(msg)
    var i = 1
    for (card <- cards) {
      println(i + ". " + card.name)
      i += 1
    }
    val input = readLine(inputPrefix).trim
    if (input == "") return null
    if (input.head.isDigit) {
      val response = input.head.toString.toInt
      if (response < 1 || response > cards.length) {
        println("Invalid input!")
        return null
      } else return cards(response - 1)
    } else {
      println("Invalid input!")
      return null
    }
  }

  /*
   * Tries to play the given card unto the field from the current player's hand.
   */
  def playCard(card: Card): Unit = {
    if (card == null) return
    if (currentPlayer.AP < card.cost)
      println("You don't have enough AP! You need " + card.cost + " AP, but you have " + currentPlayer.AP + " AP.")
    else {
      currentPlayer.AP -= card.cost
      currentPlayer.hand -= card
      if (card.cardType == "MinionCard") currentPlayer.field += card
      checkEffect(card)
    }
  }

  /*
   * Tries to attack with the given card.
   * 
   * Displays a list of attackable minions and attacks the chosen minion or 
   * attacks the other player if there are no minions to attack. 
   */
  def attackWithCard(card: Card): Unit = {
    if (card == null) return
    if (card.canAttack == false) println("This card can't attack this turn!")
    else {
      if (otherPlayer.field.length == 0) {
        otherPlayer.health -= card.currentAttack
        println("You attacked " + otherPlayer.name + "!")
        if (otherPlayer.health <= 0) gameOver(currentPlayer)
        else print(" He has now " + otherPlayer.health + " HP remaining.")
      } else {
        val taunting = otherPlayer.field.filter(_.taunt == true)
        var attackables: ListBuffer[Card] = null
        if (taunting.length > 0) attackables = taunting
        else attackables = otherPlayer.field
        val attackable = chooseCard(attackables, "Choose a card to attack: ")
        attackable.currentHealth -= card.currentAttack
        println("You attacked " + attackable.name + "! ")
        if (attackable.currentHealth <= 0) {
          print("It was destroyed!")
          otherPlayer.field -= attackable
        } else print("It has " + attackable.currentHealth + " HP left.")
      }
    }
  }

  /*
   * Displays who won and asks if the players want to play again.
   */
  def gameOver(winner: Player): Unit = {
    isGameOver = true
    println("\n" + winner.name + " has won!!")
    while (true) {
      println("Do you want to start a new game? (Y/N)")
      val input = readLine(inputPrefix).trim.toUpperCase
      if (input == "Y") {
        newGame
        return
      } else if (input == "N") return
    }
  }

  /*
   * Checks the given card's effect.
   */
  def checkEffect(card: Card): Unit = {

  }

  /*
   * Shuffles the given deck.
   */
  def shuffleDeck(deck: Deck): Unit = {
    deck.cards = shuffle(deck.cards)
  }
}