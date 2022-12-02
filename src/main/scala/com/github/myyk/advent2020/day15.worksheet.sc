import com.github.myyk._

val input = com.github.myyk.readInput(2020,15).map(_.split(",").map(_.toInt)).flatten

// plays to 'finalTurn' turn of the game and returns the value of the last turn
def playGame(finalTurn:Int): Int = {
  var whenLastSpoken = input.zipWithIndex.toMap.view.mapValues(_ + 1).toMap.withDefaultValue(0)
  var nextSpoken = 0
  for {
    turn <- (input.size + 1) until finalTurn
  } {
//    println(s"Turn $turn spoken = $nextSpoken")
    val lastSpoken = nextSpoken
    whenLastSpoken.get(lastSpoken) match {
      case None =>
        nextSpoken = 0
      case Some(lastSpokenTurn) =>
        nextSpoken = turn - lastSpokenTurn
    }

    whenLastSpoken = whenLastSpoken + (lastSpoken->turn)

//    println(s"whenLastSpoken $whenLastSpoken")
  }
  nextSpoken
}

// Answer 1
val answer1 = playGame(2020)
//289

// Answer 2
// Didn't do anything to make it more efficient, probably could use a mutable map
// to make it run a little more memory efficiently, but it completed.
val answer2 = playGame(30000000)
//1505722