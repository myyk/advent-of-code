import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

//val fileSource = false
val fileSource = true
val rawInput = if (fileSource) {
  readInput(12)
} else {
  Seq("initial state: #..#.#..##......###...###",
    "",
    "...## => #",
    "..#.. => #",
    ".#... => #",
    ".#.#. => #",
    ".#.## => #",
    ".##.. => #",
    ".#### => #",
    "#.#.# => #",
    "#.### => #",
    "##.#. => #",
    "##.## => #",
    "###.. => #",
    "###.# => #",
    "####. => #"
  )
}

var pots = Set.empty[Long]
var growRules = Set.empty[(Boolean,Boolean,Boolean,Boolean,Boolean)]

def makePots(str: String): Set[Long] = {
  var pots = Set.empty[Long]
  for {
    (next, i) <- str.zipWithIndex
    if next == '#'
  } yield {
    pots = pots + i
  }
  pots
}

val initialReg = raw"initial state: ([#.]+)".r
val growRuleReg = raw"([#.]+) => ([#.])".r

for {
  next <- rawInput
} {
  next match {
    case initialReg(potSequence) => {
      pots = makePots(potSequence)
    }
    case growRuleReg(rule, toGrow) if toGrow == "#" => {
      val result = rule.map(_ == '#')
      growRules = growRules + ((result(0), result(1), result(2), result(3), result(4)))
    }
    case _ =>
  }
}

for {
  i <- 1 to 300
//  i <- 1 to 500000000
//  j <- 1 to 100
} yield {
  val min = pots.min - 2
  val max = pots.max + 2
  pots = for {
    nextPot <- (min to max).toSet
    if growRules(pots (nextPot - 2), pots (nextPot - 1), pots (nextPot), pots (nextPot + 1), pots (nextPot + 2))
  } yield {
    nextPot
  }

  println(s"$i: ${pots.sum}")
  pots.sum
}

// Answer 1
val ans1 = pots.sum
// 3230

// Answer 2
//val ans2 = pots.sum

//val divisor = res1.tail.find(_ % 1646 == 0)
//for {
//  (next, index) <- res1.zipWithIndex
//  found <- res1.takeRight(res1.size - index - 1).find(_ % next == 0)
//} {
//  println(s"found $next is divided by $found")
//}

val diffs = res1.zip(res1.tail :+ 0L).map{case (next, prev) => prev - next}

// Every can tell that after a while every sum is 88 more than the last
val difference = 88
val scoreAt300 = 26704

val ans2 = (50000000000L - 300)*difference + scoreAt300
// 4400000000304