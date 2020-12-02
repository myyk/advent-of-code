import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val fileSource = false
//val fileSource = true
val rawInput = if (fileSource) {
  readInput(9)
} else {
//  Seq("9 players; last marble is worth 25 points")
//  Seq("10 players; last marble is worth 1618 points")
//  Seq("13 players; last marble is worth 7999 points")
//  Seq("30 players; last marble is worth 5807 points")
//  Seq("459 players; last marble is worth 71790 points")
  Seq("459 players; last marble is worth 7179000 points")
}.head

val r = raw"(\d+) players; last marble is worth (\d+) points".r

val (numPlayers, maxMarble) = rawInput match {
  case r(p, m) => (p.toInt, m.toInt)
}

var playerScores = Map.empty[Int, Long].withDefaultValue(0L)

var marbles = Vector(2, 1, 0)
for {
  i <- 3 to maxMarble
} {
  if (i % 23 == 0) {
    val player = i % numPlayers
    val score = playerScores(player) + i + marbles(marbles.size - 7)
    playerScores = playerScores + (player -> score)

    marbles = marbles.drop(marbles.size-6) ++ marbles.take(marbles.size-7)
  } else {
    marbles = (i +: marbles.drop(2)) ++ marbles.take(2)
  }
//  println(s"[${(i-1)%numPlayers+1}]   ${marbles.mkString("   ")}")
}

// Answer 1
val (player, score) = playerScores.maxBy{case (_, score) => score}
// 386151

// Answer 2
// 3211264152