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

val naturalNumbers: LazyList[Int] = 0 #:: naturalNumbers.map{ n => n + 1 }

def chineseRemainderTheorem(buses: Seq[Option[Int]]):BigInt = {
  val requiredBuses = for {
    (maybeID, index) <- buses.zipWithIndex
    id <- maybeID
  } yield {
    // We need the n_i to be 0 < n_i < id.
    // We also need to shift n_i to solve for time of index 0,
    // thus every n_i must shift to by -i.
    (id, ((id-index)%id+id)%id)
  }

  val reverseRequiredBuses = requiredBuses.sortBy(_._1).reverse

  var possibleEarliestTime: BigInt = reverseRequiredBuses.head._2
  var product: BigInt = reverseRequiredBuses.head._1

  for {
    next <- reverseRequiredBuses.tail
  } {
    val seq = naturalNumbers.take(1000).map(possibleEarliestTime + _ * product)
    val Some(nextPossibleEarliestTime) = seq.find(_ % next._1 == next._2)
    val n = (nextPossibleEarliestTime - possibleEarliestTime) / product
    println(s"n=$n, product=$product index = ${next._2}, id = ${next._1}, seq.take(2) = ${seq.take(2).toList} possibleEarliestTime = $possibleEarliestTime, nextPossibleEarliestTime = $nextPossibleEarliestTime")

    possibleEarliestTime = nextPossibleEarliestTime
    product *= next._1
  }
  possibleEarliestTime
}

def check(solution: BigInt, buses: Iterable[Option[Int]]): Unit = {
  val requiredBuses = for {
    (maybeID, index) <- buses.zipWithIndex
    id <- maybeID
  } yield {
    (id, index)
  }

  println(s"result=${solution % requiredBuses.head._1 == 0}, id=${requiredBuses.head._1}, index=0")
  for {
    (id, index) <- requiredBuses.tail
  } {
    val result = solution % id == ((id-index)%id+id)%id
    println(s"result=$result, ${solution % id} id=$id, index=$index")
  }
}

check(3417, Seq(Some(17),None,Some(13),Some(19)))
check(1261476, Seq(Some(67),Some(7),None,Some(59),Some(61)))

val earliestTime = chineseRemainderTheorem(buses)
check(earliestTime, buses)

// Answer 2
val answer2 = earliestTime
//836024966345345