import com.github.myyk.advent2018._

val input = com.github.myyk.readInput(20181).map(_.toInt)

// Answer 1
val answer1 = input.sum

var seen = Set.empty[Int]
val freq = LazyList.continually(input.to(LazyList)).flatten
val sequence = freq.scanLeft(0){case (sum, next) => sum + next}

// Answer 2
val result = sequence.find( next => {
  if (seen.contains(next)) {
    true
  } else {
    seen += next
    false
  }
})
