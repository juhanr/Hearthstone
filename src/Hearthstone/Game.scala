/*
 * Author: Juhan-Rasmus Risti 2014
 */

package Hearthstone

import collection.mutable.ListBuffer
import util.Random.shuffle
import EffectTime._
import EventEffectType._
import FilterType._

object Game extends App {
  val redDeckPath = "res/deck_red.txt"
  val blueDeckPath = "res/deck_blue.txt"
  val redPlayerName = "Red player"
  val bluePlayerName = "Blue player"
  val inputPrefix = "> "

  var redPlayer: Player = null
  var bluePlayer: Player = null
  var allCards: ListBuffer[Card] = null
  var currentPlayer: Player = null
  var otherPlayer: Player = null
  var isGameOver = false

  newGame()

  /*
   * Starts a new game, assigns default variables.
   */
  def newGame(): Unit = {
    isGameOver = false
    redPlayer = new Player(redPlayerName, Parser.parseDeck(Parser.readFile(redDeckPath)), 30, 0)
    bluePlayer = new Player(bluePlayerName, Parser.parseDeck(Parser.readFile(blueDeckPath)), 30, 0)
    allCards = redPlayer.deck.cards ++ bluePlayer.deck.cards
    shuffleDeck(redPlayer.deck)
    shuffleDeck(bluePlayer.deck)
    currentPlayer = bluePlayer
    println("===== Hearthstone =====")
    nextTurn()
  }

  /*
   * Starts the other player's turn, keeps running the action input prompting.
   */
  def nextTurn(): Unit = {
    nextPlayer()
    resetAttackRights()
    addActionPoint()
    println("\n--- " + currentPlayer.name + "'s turn ---")

    if (currentPlayer.deck.cards.length > 0) drawCard()
    else noCards()

    if (!isGameOver) {
      currentPlayer.printStats()
      val menu =
        "\nPlease choose an action:\n" +
          "1. View field\n" +
          "2. View hand\n" +
          "3. View card info\n" +
          "4. Play a card from your hand\n" +
          "5. Attack with a card\n" +
          "6. End turn\n"
      while (parseActionInput(readLine(menu + inputPrefix)) && !isGameOver) {}
      if (!isGameOver) nextTurn()
    }
  }

  /*
   * Changes whose turn it is.
   */
  def nextPlayer(): Unit = {
    if (currentPlayer == redPlayer) {
      currentPlayer = bluePlayer
      otherPlayer = redPlayer
    } else {
      currentPlayer = redPlayer
      otherPlayer = bluePlayer
    }
  }

  /*
   * Sets that the current player's played monsters can all attack again.
   */
  def resetAttackRights(): Unit = {
    currentPlayer.field.foreach(_.canAttack = true)
  }

  /*
   * Adds an action point to the current player, if he has less than 10.
   */
  def addActionPoint(): Unit = {
    if (currentPlayer.AP < 10) currentPlayer.AP += 1
  }

  /*
   * Draws a card from the current player's deck.
   */
  def drawCard(): Unit = {
    val drawnCard = currentPlayer.deck.cards.head
    currentPlayer.deck.cards = currentPlayer.deck.cards.tail
    println("Drawn card: " + drawnCard.name)
    currentPlayer.hand += drawnCard
  }

  /*
   * Scenario when the current player doesn't have any more cards;
   * reduces the player's HP by 10.
   */
  def noCards(): Unit = {
    println("You don't have any cards left in your deck. You lost 10 HP!\n")
    currentPlayer.health -= 10
    if (currentPlayer.health <= 0) gameOver(otherPlayer)
  }

  /*
   * Parses the given action input.
   * Returns whether an action should be prompted again or not.
   */
  def parseActionInput(input: String): Boolean = input.trim.head match {
    case '1' =>
      redPlayer.printField(); bluePlayer.printField(); return true
    case '2' =>
      currentPlayer.printHand(); return true
    case '3' =>
      viewCard(); return true
    case '4' => {
      if (currentPlayer.hand.length > 0)
        playCard(chooseCreature(currentPlayer.hand, "Please choose a card: ").asInstanceOf[Card])
      else println("There are no cards in your hand!")
      return true
    }
    case '5' => {
      if (currentPlayer.field.length > 0)
        attackWithCard(chooseCreature(currentPlayer.field, "Please choose a card: ").asInstanceOf[Card])
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
  def viewCard(): Unit = {
    var cardName: String = null
    val enterName = "Please enter the card name. (Type nothing to go back)\n"
    while (true) {
      cardName = readLine(enterName + inputPrefix).trim
      if (cardName == "") return
      for (card <- allCards if card.name == cardName) {
        println(card)
        return
      }
      println("Card not found!")
    }
  }

  /*
   * Displays a given list of cards/creatures, asks the player to choose one 
   * and returns the chosen card/creature.
   */
  def chooseCreature(creatures: ListBuffer[_ <: Creature], msg: String): Creature = {
    println(msg)
    for (i <- 1 to creatures.length) println(i + ". " + creatures(i - 1).name)

    val input = readLine(inputPrefix).trim
    if (input == "") return null
    if (input.head.isDigit) {
      val response = input.head.toString.toInt
      if (response < 1 || response > creatures.length) {
        println("Invalid input!")
        return null
      } else return creatures(response - 1)
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
      activateEffects(card, EffectTime.OnPlay)
      activateEffects(card, EffectTime.UntilDeath)
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
      card.canAttack = false
      if (otherPlayer.field.length == 0) attackOtherPlayer(card.attack)
      else {
        val attackableCreatures = getAttackableCreatures()
        val targetCreature = chooseCreature(attackableCreatures, "Choose a card to attack: ").asInstanceOf[Card]
        attackCreature(targetCreature, card)
      }
    }
  }

  /*
   * Attacks the other player.
   */
  def attackOtherPlayer(attack: Int): Unit = {
    otherPlayer.health -= attack
    println("You attacked " + otherPlayer.name + "!")
    if (otherPlayer.health <= 0) gameOver(currentPlayer)
    else print(" He has now " + otherPlayer.health + " HP remaining.")
  }

  /*
   * Returns a list of creatures who can be attacked.
   */
  def getAttackableCreatures(): ListBuffer[Card] = {
    val tauntingCreatures = otherPlayer.field.filter(_.taunt == true)
    if (tauntingCreatures.length > 0) return tauntingCreatures
    else return otherPlayer.field
  }

  /*
   * Attacks the given creature.
   */
  def attackCreature(target: Card, attacker: Card): Unit = {
    target.health -= attacker.attack
    println("You attacked " + target.name + "! ")
    activateEffects(target, EffectTime.OnDamage)
    if (target.health <= 0) {
      print("It was destroyed!")
      activateEffects(attacker, EffectTime.OnDeath)
      negateUntilDeathEffects(attacker)
      otherPlayer.field -= target
    } else print("It has " + target.health + " HP left.")
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
        newGame()
        return
      } else if (input == "N") return
    }
  }

  /*
   * Checks the given card's effects and activates them as needed.
   */
  def activateEffects(card: Card, time: EffectTime): Unit = {
    for (effectIndex <- 0 to card.effects.length - 1) {
      val effect = card.effects(effectIndex)
      if (effect.time == time) for (eventEffect <- effect.eventEffects)
        activateEventEffect(eventEffect, card, time, effectIndex)
    }
  }

  /*
   * Activates the given event effect. 
   * If it's an UntilDeath effect, it adds the effect's index 
   * and the affected creature's id to a separate list, 
   * so that the effect could be later negated.
   */
  def activateEventEffect(eventEffect: EventEffect, card: Card, time: EffectTime, effectIndex: Int): Unit = {
    if (eventEffect.effectType == EventEffectType.DrawCard) drawCard()
    else {
      val suitableCreatures = getSuitableCreatures(eventEffect.filters)
      if (suitableCreatures.length > 0) {
        val creaturesToAffect = getAffectableCreatures(suitableCreatures, eventEffect)
        for (creature <- creaturesToAffect) {
          if (time == EffectTime.UntilDeath) card.affectedCreatures += ((effectIndex, creature.id))
          for (creatureEffect <- eventEffect.creatureEffects)
            creatureEffectAction(card, creatureEffect, creature, activate = true)
        }
      }
    }
  }

  /*
   * Checks which creatures should be affected from the given creatures list and returns the according list.
   */
  def getAffectableCreatures(creatures: ListBuffer[Creature], eventEffect: EventEffect): ListBuffer[Creature] =
    eventEffect.effectType match {
      case EventEffectType.All => return creatures
      case EventEffectType.Choose => {
        var chosenCreature: Creature = null
        while (chosenCreature == null)
          chosenCreature = chooseCreature(creatures,
            "Please choose to whom the effect should be applied to.")
        return ListBuffer[Creature](chosenCreature)
      }
      case EventEffectType.Random => return ListBuffer[Creature](shuffle(creatures).head)
    }

  /*
   * If activate == true, the method activates the given creature effect on the given creature.
   * If it's false, it negates the given effect.
   */
  def creatureEffectAction(card: Card, creatureEffect: CreatureEffect, creature: Creature, activate: Boolean) = {
    if (creatureEffect.effectType == CreatureEffectType.Taunt && creature.isInstanceOf[Card])
      changeCreatureTaunt(card.name, creatureEffect, creature, activate)
    else {
      var changeValue = creatureEffect.effectValue
      if (creatureEffect.changeType == ChangeType.Absolute) changeValue = Math.abs(changeValue)
      if (!activate) changeValue *= -1
      if (creatureEffect.effectType == CreatureEffectType.Health)
        changeCreatureHealth(card.name, creature, changeValue)
      else if (creatureEffect.effectType == CreatureEffectType.Attack && creature.isInstanceOf[Card])
        changeCreatureAttack(card.name, creature, changeValue)
    }
  }

  /*
   * Changes the given creature's taunt.
   */
  def changeCreatureTaunt(cardName: String, creatureEffect: CreatureEffect, creature: Creature, activate: Boolean): Unit = {
    if (activate) creature.asInstanceOf[Card].taunt = creatureEffect.tauntValue
    else creature.asInstanceOf[Card].taunt = !creatureEffect.tauntValue
    println(cardName + "'s effect has changed " + creature.name + "'s taunt.")
  }

  /*
   * Changes the given creature's health.
   */
  def changeCreatureHealth(cardName: String, creature: Creature, changeValue: Int): Unit = {
    if (creature.isInstanceOf[Card]) creature.asInstanceOf[Card].health += changeValue
    else creature.asInstanceOf[Player].health += changeValue
    println(cardName + "'s effect has changed " + creature.name + "'s health.")
  }

  /*
   * Changes the given creature's attack.
   */
  def changeCreatureAttack(cardName: String, creature: Creature, changeValue: Int): Unit = {
    creature.asInstanceOf[Card].attack += changeValue
    println(cardName + "'s effect has changed " + creature.name + "'s attack.")
  }

  /*
   * Negates the effects of the given card's UntilDeath effects. 
   */
  def negateUntilDeathEffects(card: Card): Unit = {
    val creaturesInPlay = redPlayer.field ++ bluePlayer.field ++ ListBuffer[Creature](redPlayer, bluePlayer)
    for {
      affectedCreature <- card.affectedCreatures
      creatureInPlay <- creaturesInPlay
    } {
      if (affectedCreature._2 == creatureInPlay.id) {
        val effectToNegate = card.effects(affectedCreature._1)
        for {
          eventEffect <- effectToNegate.eventEffects
          creatureEffect <- eventEffect.creatureEffects
        } creatureEffectAction(card, creatureEffect, creatureInPlay, activate = false)
      }
    }
  }

  /*
   * Returns a list of creatures based on the given filters.
   */
  def getSuitableCreatures(filters: ListBuffer[Filter]): ListBuffer[Creature] = {
    var suitableCreatures = ListBuffer[Creature]()
    for (filter <- filters) {
      filter.filterType match {
        case FilterType.AnyCreature => suitableCreatures = suitableCreatures ++ currentPlayer.field ++ otherPlayer.field
        case FilterType.AnyHero => suitableCreatures = suitableCreatures ++ ListBuffer[Creature](currentPlayer, otherPlayer)
        case FilterType.AnyFriendly => suitableCreatures = suitableCreatures ++ currentPlayer.field
        case FilterType.Type => suitableCreatures = suitableCreatures ++
          (currentPlayer.field ++ otherPlayer.field).filter(_.minionType == filter.minionType)
        case FilterType.Self => suitableCreatures += currentPlayer
        case FilterType.Not => {
          val notSuitableCreatures = getSuitableCreatures(filter.subFilters)
          suitableCreatures = suitableCreatures ++ (currentPlayer.field ++ otherPlayer.field ++
            ListBuffer[Creature](currentPlayer, otherPlayer)).filter(!notSuitableCreatures.contains(_))
        }
        case FilterType.Any => {
          val suitableCreatures2 = getSuitableCreatures(filter.subFilters)
          suitableCreatures = suitableCreatures ++
            (currentPlayer.field ++ otherPlayer.field).filter(suitableCreatures2.contains(_))
        }
      }
    }

    return suitableCreatures
  }

  /*
   * Shuffles the given deck.
   */
  def shuffleDeck(deck: Deck): Unit = {
    deck.cards = shuffle(deck.cards)
  }
}