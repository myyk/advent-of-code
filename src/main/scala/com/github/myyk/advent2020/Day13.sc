import com.github.myyk.advent2020._

val input = readInput(13)

val earliestLeaveTime = input.head.toInt
val buses = for {
  next <- input(1).split(",").toSeq
} yield {
  next match {
    case "x" => None
    case id => Some(id.toInt)
  }
}

val busesToLeaveTimes = buses.flatten.map(id => id -> (id - (earliestLeaveTime % id)))
val nextBus = busesToLeaveTimes.minBy(_._2)

// Answer 1
val answer1 = nextBus._1*nextBus._2
//2935

val earliestTime:Long = ???

// Answer 2
val answer2 = ???