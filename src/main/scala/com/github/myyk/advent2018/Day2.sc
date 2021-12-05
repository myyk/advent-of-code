import com.github.myyk.advent2018._

val input = com.github.myyk.readInput(20182)

val boxIdLetterNumOccurrences = for {
  boxId <- input
} yield {
  for {
    (_, chars) <- boxId.toSeq.groupBy(next => next)
  } yield {
    chars.size
  }
}

def containsExactly(n:Int): Seq[Iterable[Int]] = for{
  next <- boxIdLetterNumOccurrences
  if next.toSet.contains(n)
} yield {
  next
}

val exactlyTwice: Int = containsExactly(2).size
val exactlyThrice: Int = containsExactly(3).size

// Answer 1
val checksum = exactlyTwice*exactlyThrice

val sortedInput = input.sorted.toIndexedSeq

val pairsToDifference = for {
  i <- input.sorted.indices
  left = sortedInput(i)
  j <- input.sorted.indices
  right = sortedInput(j)
} yield  {
  (left, right) -> (left zip right).count{ case (a,b) => a != b}
}

val Some(((boxId1, boxId2), _)) =  pairsToDifference.find{case ((_, _), difference) => difference == 1}

// Answer 2
val same = (boxId1 zip boxId2).filter{case (a,b) => a == b}.map(_._1).mkString
