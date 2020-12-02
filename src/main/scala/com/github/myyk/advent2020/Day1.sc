import com.github.myyk.advent2020._

val input = readInput(1).map(_.toInt)

val target = 2020

val inputSet = input.toSet
// O(n)
val compliment = inputSet.find(next => inputSet.contains(target - next)).get

// double check
inputSet.contains(target - compliment)
inputSet.contains(compliment)

// Answer 1
val answer1 = (target - compliment) * compliment

// Answer 2
// a. using sorted list would be fast bc we can skip values based on the first number and only need to check once for second number.
// b. iterating in O(n*n) isn't too bad maybe

val inputArray = input.toArray
val (a,b,c) = (for {
  i <- inputArray.view.dropRight(2).indices
  nextA = inputArray(i)
  j <- inputArray.view.drop(i+1).indices
  nextB = inputArray(i+1+j)
  nextC = target - nextA - nextB
  if inputSet.contains(nextC)
} yield {
  (nextA, nextB, nextC)
}).head

val answer2 = a*b*c