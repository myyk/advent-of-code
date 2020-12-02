import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val input = readInput(2)

val boxIdLetterNumOccurrences = for {
  boxId <- input
} yield {
  for {
    (_, chars) <- boxId.groupBy(next => next)
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
  i <- 0 until input.sorted.length
  left = sortedInput(i)
  j <- i until input.sorted.length
  right = sortedInput(j)
} yield  {
  (left, right) -> (left zip right).count{ case (a,b) => a != b}
}

val Some(((boxId1, boxId2), _)) =  pairsToDifference.find{case ((_, _), difference) => difference == 1}

// Answer 2
val same = (boxId1 zip boxId2).filter{case (a,b) => a == b}.map(_._1).mkString