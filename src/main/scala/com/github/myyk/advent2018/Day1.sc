import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val input = readInput(1).map(_.toInt)

// Answer 1
val answer1 = input.sum

var seen = Set.empty[Int]
val freq = Stream.continually(input.toStream).flatten
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
