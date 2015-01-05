package Hearthstone

import scala.collection.mutable.ListBuffer

object Parser {

  /*
   * Reads the content of the file at the given path
   * and returns the content.
   */
  def readFile(filePath: String): String = {
    val source = scala.io.Source.fromFile(filePath)
    val content = source.mkString
    source.close()
    return content
  }

  /*
   * Parses the given deck string and returns the according deck object.
   */
  def parseDeck(deckString: String): Deck = {
    val cards = deckString.split("\\),[\n\r]").map(stripBrackets(_, 2))
    var deck = new Deck
    cards.foreach(deck.cards += parseCard(_))
    return deck
  }

  /*
   * Parses the given card string and returns the according card object.
   */
  def parseCard(cardString: String): Card = {
    var cardAttributes = findElements(cardString)
    cardAttributes = cardAttributes.take(2) ++ findElements(cardAttributes(2).stripPrefix(",").trim, ' ')

    val effectStrings = findElements(stripBrackets(cardAttributes(3)))
    var effects = ListBuffer[Effect]()
    effectStrings.foreach(effects += parseEffect(_))

    if (cardAttributes(2) == "MinionCard") {
      new Card("MinionCard", cardAttributes(0).replace("\"", ""), cardAttributes(1).toInt, effects, cardAttributes(4).toInt,
        cardAttributes(5).toInt, cardAttributes(6).toBoolean, MinionType.valueOf(cardAttributes(7)))
    } else if (cardAttributes(2) == "SpellCard") {
      new Card("SpellCard", cardAttributes(0).replace("\"", ""), cardAttributes(1).toInt, effects)
    } else throw new ParseErrorException()
  }

  /*
   * Parses the given effect string and returns the according effect object.
   */
  def parseEffect(effectString: String): Effect = {
    val effectAttributes = findElements(effectString, ' ')
    var effect = new Effect(EffectTime.valueOf(effectAttributes(0)))
    val eventEffectStrings = findElements(stripBrackets(effectAttributes(1)))
    eventEffectStrings.foreach(effect.eventEffects += parseEventEffect(_))
    return effect
  }

  /*
   * Parses the given event effect string and returns the according event effect object.
   */
  def parseEventEffect(effectString: String): EventEffect = {
    if (effectString == EventEffectType.DrawCard.toString) {
      return new EventEffect(EventEffectType.DrawCard)
    } else {
      val effectAttributes = findElements(effectString, ' ')
      var eventEffect = new EventEffect(EventEffectType.valueOf(effectAttributes(0)))

      val filterStrings = findElements(stripBrackets(effectAttributes(1)))
      filterStrings.foreach(eventEffect.filters += parseFilter(_))

      val creatureEffectStrings = findElements(stripBrackets(effectAttributes(2)))
      creatureEffectStrings.foreach(eventEffect.creatureEffects += parseCreatureEffect(_))

      return eventEffect
    }
  }

  /*
   * Parses the given filter string and returns the according filter object.
   */
  def parseFilter(filterString: String): Filter = {
    val filterAttributes = findElements(filterString, ' ')
    val filterType = FilterType.valueOf(filterAttributes(0))
    var filter = new Filter(filterType)
    if (filterType == FilterType.Not || filterType == FilterType.Any) {
      val filterStrings = findElements(stripBrackets(filterAttributes(1)))
      filterStrings.foreach(filter.subFilters += parseFilter(_))
    } else if (filterType == FilterType.Type)
      filter.minionType = MinionType.valueOf(filterAttributes(1))
    return filter
  }

  /*
   * Parses the given creature effect string and returns the according creature effect object.
   */
  def parseCreatureEffect(effectString: String): CreatureEffect = {
    val effectAttributes = effectString.split(" ")
    if (effectAttributes(0) == CreatureEffectType.Taunt.toString)
      new CreatureEffect(CreatureEffectType.Taunt, effectAttributes(1).toBoolean)
    else
      new CreatureEffect(CreatureEffectType.valueOf(effectAttributes(0)),
        ChangeType.valueOf(effectAttributes(1)), stripBrackets(effectAttributes(2)).toInt)
  }

  /*
   * Strips the given amount of brackets from both sides of the given string.
   */
  def stripBrackets(str: String, amount: Int = 1): String = {
    var returnStr = str.trim
    if ("[(".contains(returnStr.head)) returnStr = returnStr.drop(1)
    if (")]".contains(returnStr.last)) returnStr = returnStr.dropRight(1)
    if (amount > 1) return stripBrackets(returnStr, amount - 1)
    else return returnStr.trim
  }

  /*
   * Returns a list of elements strings, which are extracted from the given list string.
   */
  def findElements(string: String, delimiter: Char = ','): ListBuffer[String] = {
    var elements = ListBuffer[String]()
    if (string != "") {
      var delimiterIndexes = findDelimiterIndexes(string, delimiter)

      val lastIndex = delimiterIndexes.length - 1
      for (i <- 0 to lastIndex) {
        val lastTwoChars = string.takeRight(2)
        if (i != lastIndex) elements += string.substring(delimiterIndexes(i), delimiterIndexes(i + 1)).stripPrefix(",").trim
        else if (lastTwoChars(0) == ' ') elements(elements.length - 1) = elements.last + lastTwoChars
        else elements(elements.length - 1) = elements.last + lastTwoChars(1)
      }
    }
    return elements
  }

  /*
   * Returns a list of indexes of the given delimiter of the given string.
   */
  def findDelimiterIndexes(string: String, delimiter: Char): ListBuffer[Int] = {
    var indexes = ListBuffer[Int](0)
    var depth = 0
    var char = ' '
    for (i <- 0 to string.length - 1) {
      char = string(i)
      if (depth == 0 && char == delimiter) indexes += i
      else if (char == '[') depth += 1
      else if (char == ']') depth -= 1
    }
    indexes += string.length - 1
    return indexes
  }

  /*
   * Exception class for parse errors.
   */
  class ParseErrorException extends java.lang.RuntimeException {
    def this(msg: String) = {
      this()
      println("Exception: " + msg)
    }
  }
}