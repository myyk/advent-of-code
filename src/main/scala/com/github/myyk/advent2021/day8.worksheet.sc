import com.github.myyk.advent2021._

val input = com.github.myyk.readInput(2021,8)

val segmentsCountToDigits = Map(
    2 -> 1,
    4 -> 4,
    3 -> 7,
    7 -> 8,
)

// oops this is actually a 7-segment display
type Segments = Set[Char]

// 7-segment display mapping
/*
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
*/

// convert these to default encoding
val defaultSegmentEncoding: Map[Segments, Int] = Map(
    Set('a','b','c','e','f','g') -> 0,
    Set('c','f') -> 1,
    Set('a','c','d','e','g') -> 2,
    Set('a','c','d','f','g') -> 3,
    Set('b','c','d','f') -> 4,
    Set('a','b','d','f','g') -> 5,
    Set('a','b','d','e','f','g') -> 6,
    Set('a','c','f') -> 7,
    Set('a','b','c','d','e','f','g') -> 8,
    Set('a','b','c','d','f','g') -> 9,
)

// convert segments to digit by using counts
def segmentsToDigit(segments: Segments): Option[Int] = {
    segmentsCountToDigits.get(segments.size)
}

// entry is a 10-digit unique signal pattern and 4-digit output value
case class Entry(signal: Seq[Segments], output: Seq[Segments]) {
    // output some digits
    def someDigits: Seq[Int] = output.map(segmentsToDigit).flatten

    lazy val allSegments = signal ++ output

    // scrabbled encoding is the encoding for the segments
    lazy val scrabbledEncoding: Map[Char, Char] = {
        // if there's a size 2 and size 3 segments in all segements, then find the disjoint set
        val aWithCAndF = for {
            one <- allSegments.find(_.size == 2) 
            seven <- allSegments.find(_.size == 3)
        } yield {
            ((seven diff one), (seven intersect one))
        }

        val a = for {
            (a, _) <- aWithCAndF
        } yield {
            assert(a.size == 1)
            a.head
        }

        // if there's a 4 segments in all segments, then the difference between that and cAndF is bAndD
        val bAndD = for {
            four <- allSegments.find(_.size == 4)
            (_, cAndF) <- aWithCAndF
        } yield {
            four diff cAndF
        }

        // 0 6 9 are all 6 segments, find all unique 6 segments and if there are only 3, then diff with 8. That will be cAndDandE
        val unique6Segments = allSegments.filter(_.size == 6).toSet
        val cAndDandE = for {
            eight <- allSegments.find(_.size == 7)
            if unique6Segments.size == 3
        } yield {
            // intersect all 6 segments
            val intersecting6 = unique6Segments.reduce(_ intersect _)
            eight diff intersecting6
        }

        val c = for {
            (_, cAndF) <- aWithCAndF
            cAndDandE <- cAndDandE
        } yield {
            val c = cAndF intersect cAndDandE
            assert(c.size == 1)
            c.head
        }

        // b+d - c+d+e = b
        val b = for {
            bAndD <- bAndD
            cAndDandE <- cAndDandE
        } yield {
            val b = bAndD diff cAndDandE
            assert(b.size == 1)
            b.head
        }

        val d = for {
            bAndD <- bAndD
            b <- b
        } yield {
            val d = bAndD - b
            assert(d.size == 1)
            d.head
        }

        // c+d+e - c+d = e
        val e = for {
            cAndDandE <- cAndDandE
            c <- c
            d <- d
        } yield {
            val e = cAndDandE diff Set(c,d)
            assert(e.size == 1)
            e.head
        }

        // c+f - c = f
        val f = for {
            (_, cAndF) <- aWithCAndF
            c <- c
        } yield {
            val f = cAndF - c
            assert(f.size == 1)
            f.head
        }

        // a+b+c+d+e+f+g - a+b+c+d+e+f = g
        val g = for {
            a <- a
            b <- b
            c <- c
            d <- d
            e <- e
            f <- f
        } yield {
            val g = Set[Char]('a','b','c','d','e','f','g') diff Set(a,b,c,d,e,f)
            assert(g.size == 1)
            g.head
        }

        // make the result mapping of variables to their letters
        val result = Map(
            a.head -> 'a',
            b.head -> 'b',
            c.head -> 'c',
            d.head -> 'd',
            e.head -> 'e',
            f.head -> 'f',
            g.head -> 'g',
        )
        assert(result.size == 7, s"${result.size} expected 7, values are ${result.values}")
        result
    }

    // descrable output
    def descrambleOutput: Int = {
        val descrambledOutputSegments = output.map(_.map(scrabbledEncoding))
        // covert descrambled output segments into digits
        val descrambledOutputDigits = descrambledOutputSegments.map(defaultSegmentEncoding)
        // reverse and scale up with powers of 10
        descrambledOutputDigits.reverse.zipWithIndex.map { case (digit, index) => digit * math.pow(10, index).toInt }.sum
    }
}

// convert input to entries that look like: "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
def inputToEntries(input: Seq[String]): Seq[Entry] = {
    input.map { line =>
        val parts = line.split('|')
        val signal = parts(0).trim.split(" ").toSeq.map(_.toSet)
        val output = parts(1).trim.split(" ").toSeq.map(_.toSet)
        Entry(signal, output)
    }
}

// construct entry from "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
val testEntry = Entry("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab".split(" ").toSeq.map(_.toSet),
    "cdfeb fcadb cdfeb cdbaf".split(" ").toSeq.map(_.toSet))
val parsedEntry = inputToEntries(Seq("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
).head
assert(testEntry == parsedEntry)

// convert entry from "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
val testEntry2 = Entry("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb".split(" ").toSeq.map(_.toSet),
    "fdgacbe cefdb cefbgd gcbe".split(" ").toSeq.map(_.toSet))
assert(testEntry2.someDigits.toSet == Set(4,8))

val entries = inputToEntries(input)

// find all the output digits
val allDigits = entries.flatMap(_.someDigits)

val answer1 = allDigits.size
//330

val testOutput = testEntry.descrambleOutput
assert(testOutput == 5353)

// add all entry's descrambled outputs together
val answer2 = entries.map(_.descrambleOutput).sum
// 1010472