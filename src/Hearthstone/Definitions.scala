package Hearthstone

import collection.mutable.ListBuffer

case class Player(val name: String, var deck: Deck, var health: Int, var AP: Int) {
  var field = ListBuffer[Card]()
  var hand = ListBuffer[Card]()

  def printStats: Unit = println("Health: " + health + "\nAction Points: " + AP)

  def printField: Unit = {
    println("- " + name + " player's field -")
    if (field.length == 0) println("Empty")
    else for (card <- field) println(card.name)
  }

  def printHand: Unit = {
    println("- " + name + " player's hand -")
    if (hand.length == 0) println("Empty")
    else for (card <- hand) println(card.name)
  }
}

case class Deck {
  var cards = ListBuffer[Card]();

  override def toString: String = {
    var result = "Deck: ["
    for (card <- cards) result += card + ", "
    return result.stripSuffix(", ") + "]"
  }
}

sealed abstract class Card(val name: String, val cost: Int, val effects: ListBuffer[Effect])

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

case class MinionCard(override val name: String, override val cost: Int, val health: Int, val attack: Int,
  val taunt: Boolean, val minionType: MinionType, override val effects: ListBuffer[Effect])
  extends Card(name: String, cost: Int, effects: ListBuffer[Effect]) {
  var currentHealth = health
  var currentAttack = attack
  var currentTaunt = taunt

  override def toString: String = {
    var result = "MinionCard: {"
    val keys = List[String]("Name", "Cost", "Health", "Attack", "Taunt", "MinionType", "Effects")
    val values = this.productIterator.toList
    for (attributes <- keys zip values) {
      if (attributes._1 == "Effects") {
        result += attributes._1 + ": ["
        for (effect <- effects) {
          result += effect + ", "
        }
        result = result.stripSuffix(", ") + "]"
      } else result += attributes._1 + ": " + attributes._2 + ", "
    }
    return result.stripSuffix(", ") + "}"
  }
}

case class SpellCard(override val name: String, override val cost: Int, override val effects: ListBuffer[Effect])
  extends Card(name: String, cost: Int, effects: ListBuffer[Effect]) {

  override def toString: String = {
    var result = "SpellCard: {"
    val keys = List[String]("Name", "Cost", "Effects")
    val values = this.productIterator.toList
    val spacing = "    "
    for (attributes <- keys zip values) {
      if (attributes._1 == "Effects") {
        result += attributes._1 + ": ["
        for (effect <- effects) {
          result += effect + ", "
        }
        result = result.stripSuffix(", ") + "]"
      } else result += attributes._1 + ": " + attributes._2 + ", "
    }
    return result.stripSuffix(", ") + "}"
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
    var result = "Effect: {"
    result += "EffectTime: " + time + ", EventEffects: ["
    for (eventEffect <- eventEffects) {
      result += eventEffect + ", "
    }
    result = result.stripSuffix(", ") + "]"
    return result.stripSuffix(", ") + "}"
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
    case _ => throw new Parser.ParseErrorException()
  }
}

import EventEffectType._

case class EventEffect(val effectType: EventEffectType) {
  var filters = ListBuffer[Filter]()
  var creatureEffects = ListBuffer[CreatureEffect]()

  override def toString: String = {
    var result = "EventEffect: {EventEffectType: " + effectType

    if (effectType != EventEffectType.DrawCard) {
      result += ", Filters: ["
      for (filter <- filters) {
        result += filter + ", "
      }
      result = result.stripSuffix(", ") + "], CreatureEffects: ["

      for (creatureEffect <- creatureEffects) {
        result += creatureEffect + ", "
      }
      result = result.stripSuffix(", ") + "]"
    }
    return result.stripSuffix(", ") + "}"
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
    var result = "CreatureEffect: {EffectType: " + effectType + ", "
    if (effectType == CreatureEffectType.Taunt) {
      result += "Taunt: " + tauntValue
    } else {
      result += "ChangeType: " + changeType + ", EffectValue: " + effectValue
    }
    return result.stripSuffix(", ") + "}"
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
    var result = "Filter: {FilterType: " + filterType + ", "
    if (filterType == FilterType.Type) {
      result += "MinionType: " + minionType + ", "
    } else if (filterType == FilterType.Any || filterType == FilterType.Not) {
      result += "SubFilters: ["
      for (subfilter <- subFilters) {
        result += subfilter + ", "
      }
      result = result.stripSuffix(", ") + "]"
    }
    return result.stripSuffix(", ") + "}"
  }
}