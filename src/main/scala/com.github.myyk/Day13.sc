import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val fileSource = false
//val fileSource = true
val rawInput = if (fileSource) {
  readInput(13)
} else {
  Seq()
}
