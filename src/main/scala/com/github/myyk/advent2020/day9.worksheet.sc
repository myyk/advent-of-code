import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(2020,9).toVector.map(_.toLong)

val preambleLen = 25

val encodedAndValidation = for {
  sliding <- input.view.sliding(preambleLen + 1)
} yield {
  // could be optimized to not create whole set each time but this is quick and readable
  val preamble = sliding.take(preambleLen).toSet
  val next = sliding.last

  // could be optimized if sorted, but why bother for n=25?
  val isMatchingXMAS = preamble.exists(x => preamble.contains(next - x))
  (next, isMatchingXMAS)
}

// Answer 1
val answer1 = encodedAndValidation.find(!_._2).get._1
//466456641

val target = answer1
var i = 0
var j = 1
var sum = input.head
while (j < input.size && sum != target) {
  if (sum < target) {
    sum += input(j)
    j += 1
  } else {
    sum -= input(i)
    i += 1
  }
}

val encryptionWeakness = input.slice(i, j)

// Answer 2
val answer2 = encryptionWeakness.min + encryptionWeakness.max
//55732936