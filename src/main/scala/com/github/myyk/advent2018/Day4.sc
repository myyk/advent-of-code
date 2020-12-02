import com.github.myyk.advent2018._

val inputs = readInput(4).sorted

val entry = raw"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)".r
val shiftBeginSummary = raw"Guard #(\d+) begins shift".r

var currentGuard = 0
var timeAsleep = 0
// id to awake and asleep pairs
var guardTimesAsleep = Map.empty[Int, Seq[(Int, Int)]].withDefaultValue(Seq.empty)

for {
  entry(_,_,_,_,mins,summary) <- inputs
} {
  summary match {
    case shiftBeginSummary(id) =>
      currentGuard = id.toInt
    case "falls asleep" =>
      timeAsleep = mins.toInt
    case "wakes up" =>
      val sleepTime = (timeAsleep, mins.toInt)
      val sleepTimes = guardTimesAsleep(currentGuard) :+ sleepTime
      guardTimesAsleep = guardTimesAsleep + (currentGuard -> sleepTimes)
  }
}

val guardTotalSleepTimes = for {
  (guard, sleepTimes) <- guardTimesAsleep
} yield {
  val values = for {
    (start, end) <- sleepTimes
  } yield {
    end - start
  }
  guard -> values.sum
}

val (sleepiestID, _) = guardTotalSleepTimes.maxBy(_._2)

val sleepiestGuardSleepTimes = guardTimesAsleep(sleepiestID)

def guardMostSleepyMinuteAndFreq(sleepTimes: Seq[(Int, Int)]): (Int, Int) = {
  val minutesAsleep = Array.fill(60)(0)
  for {
    (sleep, wake) <- sleepTimes
    i <- sleep until wake
  } {
    minutesAsleep(i) +=  1
  }

  val (freq, minAsleepMost) = minutesAsleep.zipWithIndex.maxBy(_._1)
  (minAsleepMost, freq)
}

val (minAsleepMost, freq) = guardMostSleepyMinuteAndFreq(sleepiestGuardSleepTimes)

// Answer 1
val ans1 = sleepiestID * minAsleepMost
// 36898

val guardSleepiestTimes = for {
  (guard, sleepTimes) <- guardTimesAsleep
} yield {
  guard -> guardMostSleepyMinuteAndFreq(sleepTimes)
}

val (sleepiestGuard2, (sleepiestMin2, sleepiestFreq2)) = guardSleepiestTimes.maxBy(_._2._2)

// Answer 2
val ans2 = sleepiestGuard2 * sleepiestMin2
