import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(2020,19)

val (_, emptyIndex) = input.zipWithIndex.find(_._1.isEmpty).get
val (ruleStrs, messagesStrs) = input.splitAt(emptyIndex)
val messages = messagesStrs.map(_.toList)

sealed trait Rule {
  def isMatched(str: List[Char]): Boolean
  def minSize: Int
  def maxSize: Int
}
case class Pattern(a: Rule, b: Rule) extends Rule {
  def isMatched(str: List[Char]): Boolean = {
    // Improved by keeping min/max size for each rule
    if (minSize > str.size || str.size > maxSize) {
      false
    } else {
      // This is probably not the smartest way of doing this
      (a.minSize to str.size-b.minSize).exists { i =>
        val (left, right) = str.splitAt(i)
        a.isMatched(left) && b.isMatched(right)
      }
    }
  }

  val minSize: Int = a.minSize + b.minSize
  val maxSize: Int = a.maxSize + b.maxSize
}
case class Or(a: Rule, b: Rule) extends Rule {
  def isMatched(str: List[Char]): Boolean = {
    a.isMatched(str) || b.isMatched(str)
  }
  val minSize: Int = a.minSize min b.minSize
  val maxSize: Int = a.maxSize max b.maxSize
}
case class Literal(c: Char) extends Rule {
  def isMatched(str: List[Char]): Boolean = {
    str.size == 1 && str.head == c
  }
  val minSize: Int = 1
  val maxSize: Int = 1
}

val ruleRegex = raw"(\d+): (.*)".r
val literalRegex = raw"(\w)".r
val replacementRegex = raw"(\d+)".r
val patternRegex = raw"(\d+) (\d+)".r
val orRegex2 = "([\\d]+) \\| ([\\d]+)".r
val orRegex4 = "([\\d]+) ([\\d]+) \\| ([\\d]+) ([\\d]+)".r

val ruleToDefinition = {for {
  next <- ruleStrs
} yield {
  next match {
    case ruleRegex(ruleID, definition) => ruleID.toInt -> definition.replaceAll("\"", "").trim
  }
}}.toMap

// parseRule the new rule and adds it to the map
def parseRule(id: Int, rules: Map[Int, Rule]): Map[Int, Rule] = {
  println(s"id = $id")
  rules.get(id) match {
    case Some(rule) =>
      println(s"found = $rule")
      rules
    case None =>
      println(s"next = <${ruleToDefinition(id)}>")
      val newRules = ruleToDefinition(id) match {
        case literalRegex(charStr) =>
          rules + (id -> Literal(charStr.head))
        case replacementRegex(aID) =>
          val updatedRules = parseRule(aID.toInt, rules)
          updatedRules + (id.toInt -> updatedRules(aID.toInt))
        case patternRegex(aID, bID) =>
          val updatedRules = Seq(aID, bID).map(_.toInt).foldLeft(rules) {
            case (nextRules, id) => parseRule(id, nextRules)
          }
          updatedRules + (id -> Pattern(updatedRules(aID.toInt), updatedRules(bID.toInt)))
        case orRegex2(aID, bID) =>
          val updatedRules = Seq(aID, bID).map(_.toInt).foldLeft(rules) {
            case (nextRules, id) => parseRule(id, nextRules)
          }
          updatedRules + (id -> Or(
            updatedRules(aID.toInt),
            updatedRules(bID.toInt),
          ))
        case orRegex4(aID, bID, cID, dID) =>
          val updatedRules = Seq(aID, bID, cID, dID).map(_.toInt).foldLeft(rules) {
            case (nextRules, id) => parseRule(id, nextRules)
          }
          updatedRules + (id -> Or(
            Pattern(updatedRules(aID.toInt), updatedRules(bID.toInt)),
            Pattern(updatedRules(cID.toInt), updatedRules(dID.toInt)),
          ))
      }
      println(s"returning id = $id")
      println(s"id = $id, added = <${newRules(id)}>")
      newRules
  }
}

val rules = parseRule(0, Map.empty[Int, Rule])
val rule0 = rules(0)

// Answer 1
val answer1 = messages.count(message => rule0.isMatched(message))
//291

// Note changing rule 8 only affects rule 8
// "8: 42" changes to "8: 42 | 42 8"
// for rule 42 min and max size is 8

// Note changing rule 11 only affects rule 11
// "11: 42 31" changes to "11: 42 31 | 42 11 31"
// for rule 42 min and max size is 8

case class RuleAlt0(a: Rule, b: Rule) extends Rule {
  assert(a.minSize == a.maxSize)
  assert(b.minSize == b.maxSize)

  val aSize = a.maxSize
  val bSize = b.maxSize
  val minSize: Int = a.minSize*2 + b.minSize
  val maxSize: Int = Int.MaxValue

  final def isMatched(str: List[Char]): Boolean = {
    val maxAMatchesFromLeft = countAFromLeft(str)
    val maxBMatchesFromRight = countBFromRight(str)

//    println(s"a = $maxAMatchesFromLeft, b = $maxBMatchesFromRight")

    (2 to maxAMatchesFromLeft).exists{i=>
      val remaining = str.size - (i * aSize)
      val bMatches = remaining/bSize
      if (remaining%bSize != 0) {
        false
      } else if (1 > bMatches || bMatches > maxBMatchesFromRight) {
        false
      } else {
        i > bMatches
      }
    }
  }

  def countAFromLeft(str: List[Char]): Int = {
    str.grouped(aSize).filter(_.size == aSize).takeWhile(s => a.isMatched(s)).size
  }
  def countBFromRight(str: List[Char]): Int = {
    str.reverse.grouped(bSize).filter(_.size == bSize).map(_.reverse).takeWhile(s => b.isMatched(s)).size
  }
}

val newRule0 = RuleAlt0(rules(42), rules(31))

rules(42).maxSize
rules(31).maxSize

messages.filter(message => newRule0.isMatched(message)).map(n => println(n.mkString("")))

// Answer 2
val answer2 = messages.count(message => newRule0.isMatched(message))
//409