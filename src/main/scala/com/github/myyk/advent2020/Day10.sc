import com.github.myyk.advent2020._
import scala.collection.mutable

val input = com.github.myyk.readInput(202010).map(_.toInt)

val joltageTolerance = 3
val deviceJoltage = input.max + 3

val sortedAdapters = 0 +: input.sorted :+ deviceJoltage

val joltageDifferances = sortedAdapters.sliding(2).map(next => next(1) - next(0))
val groupedDifferences = joltageDifferances.toList.groupBy(diff => diff).view.mapValues(_.size).toMap

val diff1 = groupedDifferences(1)
val diff3 = groupedDifferences(3)
// Answer 1
val answer1 = groupedDifferences(1) * groupedDifferences(3)
// 2232

// This is confusing:
// There is 1 path from start (0) to all paths it can reach
// which can be 1,2,3. From then on, for each consecutive
// adapter, there are N paths to it which add N paths to the
// next up to 3 joltage adapters.
val combinations = mutable.Map(0 -> 1L).withDefaultValue(0L)
val adapters = sortedAdapters.toSet
for {
  prev <- sortedAdapters
  next <- (prev+1) to (prev+joltageTolerance)
  if adapters.contains(next)
} {
  combinations.put(next,combinations(next)+combinations(prev))
}

// Answer 2
val answer2 = combinations(deviceJoltage)
//173625106649344