package Hearthstone

import collection.mutable.ListBuffer

object idManager {
  var id = 0
}

sealed abstract class Creature(val name: String) {
  val id: Int = idManager.id
  idManager.id += 1
}

case class Player(override val name: String, var deck: Deck, var health: Int, var AP: Int) extends Creature(name: String) {
  var field = ListBuffer[Card]()
  var hand = ListBuffer[Card]()

  def printStats(): Unit = println("Health: " + health + "\nAction Points: " + AP)

  def printField(): Unit = {
    println("- " + name + "'s field -")
    if (field.length == 0) println("Empty")
    else for (card <- field) println(card.name)
  }

  def printHand(): Unit = {
    println("- " + name + "'s hand -")
    if (hand.length == 0) println("Empty")
    else for (card <- hand) println(card.name)
  }
}

case class Deck() {
  var cards = ListBuffer[Card]();

  override def toString: String = {
    var string = "Deck: ["
    for (card <- cards) string += card + ", "
    return string.stripSuffix(", ") + "]"
  }
}

object MinionType extends Enumeration {
  type MinionType = Value
  val Beast = Value("Beast")
  val Murloc = Value("Murloc")

  def valueOf(arg: String) = arg match {
    case "Beast" => Beast
    case "Murloc" => Murloc
    case "Nothing" => null
    case _ => throw new Parser.ParseErrorException()
  }
}

import MinionType._

case class Card(val cardType: String, override val name: String, val cost: Int, val effects: ListBuffer[Effect],
  var health: Int, var attack: Int, var taunt: Boolean, val minionType: MinionType) extends Creature(name: String) {
  var canAttack: Boolean = false
  var affectedCreatures = ListBuffer[(Int, Int)]() // (Effect index, Creature's card id)

  def this(cardType: String, name: String, cost: Int, effects: ListBuffer[Effect]) {
    this(cardType, name, cost, effects, 0, 0, false, null)
  }

  override def toString: String = {
    var string = "Card: {Type: " + cardType + ", Name: " + name + ", Cost: " + cost + ", Effects: ["
    for (effect <- effects) string += effect + ", "
    string = string.stripSuffix(", ") + "]"
    if (cardType == "MinionCard")
      string += ", Health: " + health + ", Attack: " + attack + ", Taunt: " + taunt + ", MinionType: " + minionType
    return string + "}"
  }
}

object EffectTime extends Enumeration {
  type EffectTime = Value
  val OnPlay = Value("OnPlay")
  val UntilDeath = Value("UntilDeath")
  val OnDamage = Value("OnDamage")
  val OnDeath = Value("OnDeath")

  def valueOf(arg: String) = arg match {
    case "OnPlay" => OnPlay
    case "UntilDeath" => UntilDeath
    case "OnDamage" => OnDamage
    case "OnDeath" => OnDeath
    case _ => throw new Parser.ParseErrorException()
  }
}

import EffectTime._

case class Effect(val time: EffectTime) {
  var eventEffects = ListBuffer[EventEffect]()

  override def toString: String = {
    var string = "Effect: {"
    string += "EffectTime: " + time + ", EventEffects: ["
    for (eventEffect <- eventEffects) string += eventEffect + ", "
    string = string.stripSuffix(", ") + "]"
    return string + "}"
  }
}

object EventEffectType extends Enumeration {
  type EventEffectType = Value
  val All = Value("All")
  val Choose = Value("Choose")
  val Random = Value("Random")
  val DrawCard = Value("DrawCard")

  def valueOf(arg: String) = arg match {
    case "All" => All
    case "Choose" => Choose
    case "Random" => Random
    case "DrawCard" => DrawCard
    case _ => println(arg); throw new Parser.ParseErrorException()
  }
}

import EventEffectType._

case class EventEffect(val effectType: EventEffectType) {
  var filters = ListBuffer[Filter]()
  var creatureEffects = ListBuffer[CreatureEffect]()

  override def toString: String = {
    var string = "EventEffect: {EventEffectType: " + effectType

    if (effectType != EventEffectType.DrawCard) {
      string += ", Filters: ["
      for (filter <- filters) string += filter + ", "
      string = string.stripSuffix(", ") + "], CreatureEffects: ["

      for (creatureEffect <- creatureEffects) string += creatureEffect + ", "
      string = string.stripSuffix(", ") + "]"
    }
    return string + "}"
  }
}

object CreatureEffectType extends Enumeration {
  type CreatureEffectType = Value
  val Health = Value("Health")
  val Attack = Value("Attack")
  val Taunt = Value("Taunt")

  def valueOf(arg: String) = arg match {
    case "Health" => Health
    case "Attack" => Attack
    case "Taunt" => Taunt
    case _ => throw new Parser.ParseErrorException()
  }
}

object ChangeType extends Enumeration {
  type ChangeType = Value
  val Relative = Value("Relative")
  val Absolute = Value("Absolute")

  def valueOf(arg: String) = arg match {
    case "Relative" => Relative
    case "Absolute" => Absolute
    case _ => throw new Parser.ParseErrorException()
  }
}

import CreatureEffectType._
import ChangeType._

case class CreatureEffect(val effectType: CreatureEffectType, val changeType: ChangeType,
  val effectValue: Int, val tauntValue: Boolean) {
  def this(effectType: CreatureEffectType, changeType: ChangeType, effectValue: Int) {
    this(effectType, changeType, effectValue, false)
  }
  def this(effectType: CreatureEffectType, tauntValue: Boolean) {
    this(effectType, null, 0, tauntValue)
  }

  override def toString: String = {
    var string = "CreatureEffect: {EffectType: " + effectType + ", "
    if (effectType == CreatureEffectType.Taunt) string += "Taunt: " + tauntValue
    else string += "ChangeType: " + changeType + ", EffectValue: " + effectValue
    return string + "}"
  }
}

object FilterType extends Enumeration {
  type FilterType = Value
  val AnyCreature = Value("AnyCreature")
  val AnyHero = Value("AnyHero")
  val AnyFriendly = Value("AnyFriendly")
  val Type = Value("Type")
  val Self = Value("Self")
  val Not = Value("Not")
  val Any = Value("Any")

  def valueOf(arg: String) = arg match {
    case "AnyCreature" => AnyCreature
    case "AnyHero" => AnyHero
    case "AnyFriendly" => AnyFriendly
    case "Type" => Type
    case "Self" => Self
    case "Not" => Not
    case "Any" => Any
    case _ => throw new Parser.ParseErrorException()
  }
}

import FilterType._

case class Filter(val filterType: FilterType) {
  var minionType: MinionType = null
  var subFilters = ListBuffer[Filter]()

  override def toString: String = {
    var string = "Filter: {FilterType: " + filterType + ", "
    if (filterType == FilterType.Type) string += "MinionType: " + minionType
    else if (filterType == FilterType.Any || filterType == FilterType.Not) {
      string += "SubFilters: ["
      for (subfilter <- subFilters) string += subfilter + ", "
      string = string.stripSuffix(", ") + "]"
    }
    return string + "}"
  }
}