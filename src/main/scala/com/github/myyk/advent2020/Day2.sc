import com.github.myyk.advent2020._

val input = readInput(2)

val r = raw"(\d+)-(\d+) (\w): (\w+)".r

case class Entry(firstReq: Int, secondReq: Int, requiredChar: Char, password: String)

val inputs = input.map{ _ match {
  case r(min, max, requiredChar, password) => Entry(min.toInt, max.toInt, requiredChar.head, password)
}}

val validPasswords1 = inputs.count{ entry =>
  val occurances = entry.password.count(_ == entry.requiredChar)
  entry.firstReq <= occurances && occurances <= entry.secondReq
}

// Answer 1
val answer1 = validPasswords1

val validPasswords2 = inputs.count{ entry =>
  // first and second requirements are 1-indexed, not 0-indexed
  val a = entry.password.charAt(entry.firstReq-1)
  val b = entry.password.charAt(entry.secondReq-1)
  a == entry.requiredChar ^ b == entry.requiredChar
}

// Answer 2
val answer2 = validPasswords2