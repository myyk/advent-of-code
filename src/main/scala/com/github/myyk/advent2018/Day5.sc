import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val inputs = readInput(5).head.toVector

def reactOnce(in: Vector[Char]): Vector[Char] = {
  var nextInput = Vector.empty[Char]
  var queue = List.empty[Char]

  for {
    next <- in
  } {
    if (queue.size < 2) {
      queue = queue :+ next
    } else {
      nextInput = nextInput :+ queue.head
      queue = queue.tail.head :: next :: Nil
    }

    if (queue.size == 2) {
      val a = queue.head
      val b = queue.tail.head
      if (a.isLower != b.isLower && a.toLower == b.toLower) {
        queue = Nil
      }
    }
  }

  nextInput ++ queue
}

var prev = inputs
//var prev = "dabAcCaCBAcCcaDA".toVector

def fullyReact(in: Vector[Char]): Vector[Char] = {
  var prev = in
  var next = reactOnce (prev)
  while (prev.size != next.size) {
    prev = next
    next = reactOnce (prev)
  }
  next
}

// Answer 1
val unitsRemaining = fullyReact(inputs).size
//11546

def makeExclusionaryPolymers(in: Vector[Char]): Map[Char, Vector[Char]] = {
  val charSet = in.toSet[Char].map(_.toLower)

  val exclusionaryPolymers = for {
    c <- charSet
  } yield {
    val polymerExludingC = in.filterNot(n => n == c || n == c.toUpper)
    (c -> fullyReact(polymerExludingC))
  }
  exclusionaryPolymers.toMap
}

//val polymer = "dabAcCaCBAcCcaDA".toVector
val polymer = inputs
val smallestPolymer = makeExclusionaryPolymers(polymer)

// Answer 2
val smallestPolymerSize = smallestPolymer.values.map(_.size).min
//5124