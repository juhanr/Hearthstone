package Hearthstone

import scala.collection.mutable.ListBuffer

object Parser {

  def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val content = source.mkString
    source.close()
    return content
  }

  def parseDeck(deckString: String): Deck = {
    val cards = deckString.split("\\),[\n\r]").map(_.trim.dropWhile(" [(".contains(_)).
      reverse.trim.dropWhile(" )]\n".contains(_)).reverse)
    var deck = new Deck
    cards.foreach(deck.cards += parseCard(_))
    return deck
  }

  def parseCard(cardString: String): Card = {
    var cardAttributes = cardString.split(",").map(_.trim);
    val cardType = cardAttributes.drop(2).mkString(", ").reverse.dropWhile(_ != ']').reverse.span(_ != '[')

    val effectStrings = findElements(stripBrackets(cardType._2))
    var effects = ListBuffer[Effect]()
    effectStrings.foreach(effects += parseEffect(_))

    if (cardType._1.trim == "MinionCard") {
      cardAttributes = Array(cardAttributes(0).replace("\"", ""), cardAttributes(1)) ++ cardAttributes.last.split(" ").reverse.take(4).reverse
      new MinionCard(cardAttributes(0), cardAttributes(1).toInt, cardAttributes(2).toInt, cardAttributes(3).toInt,
        cardAttributes(4).toBoolean, MinionType.valueOf(cardAttributes(5)), effects)
    } else if (cardType._1.trim == "SpellCard") {
      new SpellCard(cardAttributes(0), cardAttributes(1).toInt, effects)
    } else throw new ParseErrorException()
  }

  def stripBrackets(str: String): String = {
    var returnStr = str.trim
    if ("[(".contains(returnStr.head)) returnStr = returnStr.drop(1)
    if (")]".contains(returnStr.last)) returnStr = returnStr.dropRight(1)
    return returnStr.trim
  }

  def parseEffect(effectString: String): Effect = {
    val effectType = effectString.span(x => x != '[')
    var effect = new Effect(EffectTime.valueOf(effectType._1.trim))
    val eventEffectStrings = findElements(stripBrackets(effectType._2))
    eventEffectStrings.foreach(effect.eventEffects += parseEventEffect(_))
    return effect
  }

  def parseEventEffect(effectString: String): EventEffect = {
    if (effectString == EventEffectType.DrawCard.toString) {
      return new EventEffect(EventEffectType.DrawCard)
    } else {
      val effectType = effectString.span(x => x != '[')
      var eventEffect = new EventEffect(EventEffectType.valueOf(effectType._1.trim))

      val effectAttributes = effectType._2.splitAt((effectType._2 indexOf "] [") + 1)

      val filterStrings = findElements(stripBrackets(effectAttributes._1))
      filterStrings.foreach(eventEffect.filters += parseFilter(_))

      val creatureEffectStrings = findElements(stripBrackets(effectAttributes._2))
      creatureEffectStrings.foreach(eventEffect.creatureEffects += parseCreatureEffect(_))

      return eventEffect
    }
  }

  def parseFilter(filterString: String): Filter = {
    val filterAttributes = filterString.span(x => x != '[')
    val attributes2 = filterAttributes._1.trim.split(" ")
    val filterType = FilterType.valueOf(attributes2(0))
    var filter = new Filter(filterType)
    if (filterType == FilterType.Not || filterType == FilterType.Any) {
      val filterStrings = findElements(stripBrackets(filterAttributes._2))
      filterStrings.foreach(filter.subFilters += parseFilter(_))
      return filter
    } else if (filterType == FilterType.Type) {
      filter.minionType = MinionType.valueOf(attributes2(1))
      return filter
    } else return filter
  }

  def parseCreatureEffect(effectString: String): CreatureEffect = {
    val effectAttributes = effectString.split(" ")
    if (effectAttributes(0) == CreatureEffectType.Taunt.toString)
      new CreatureEffect(CreatureEffectType.Taunt, effectAttributes(1).toBoolean)
    else
      new CreatureEffect(CreatureEffectType.valueOf(effectAttributes(0)),
        ChangeType.valueOf(effectAttributes(1)), stripBrackets(effectAttributes(2)).toInt)
  }

  def findElements(str: String): ListBuffer[String] = {
    var elements = ListBuffer[String]()
    if (str != "") {
      var indexes = ListBuffer[Int](0)
      var depth = 0
      var char = ' '
      for (i <- 0 to str.length - 1) {
        char = str(i)
        if (depth == 0 && char == ',') indexes += i
        else if (char == '[') depth += 1
        else if (char == ']') depth -= 1
      }
      indexes += str.length - 1

      val lastIndex = indexes.length - 1
      for (i <- 0 to lastIndex) {
        if (i != lastIndex)
          elements += str.substring(indexes(i), indexes(i + 1)).stripPrefix(",").trim
        else
          elements(elements.length - 1) = elements.last + str.last
      }
    }
    return elements
  }

  class ParseErrorException extends java.lang.RuntimeException {
    def this(msg: String) = {
      this()
      println("Exception: " + msg)
    }
  }
}