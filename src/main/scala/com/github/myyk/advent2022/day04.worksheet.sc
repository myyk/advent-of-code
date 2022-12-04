import com.github.myyk._

val input = readInput(2022,4)

// A contains B
def rangeContainsRange(a: Range, b: Range): Boolean = {
    a.contains(b.start) && a.contains(b.end)
}

def oneOfRangesIncludesOther(a: Range, b: Range): Boolean = {
    rangeContainsRange(a, b) || rangeContainsRange(b, a) 
}

val assignments = for {
    line <- input
    assignment <- line.split(',')
} yield {
    val splits = assignment.split('-')
    Range.inclusive(splits(0).toInt, splits(1).toInt)
}

val answer1 = assignments.grouped(2).count{ case Seq(a, b) => oneOfRangesIncludesOther(a, b)}
// 534

def overlap(a: Range, b: Range): Boolean = {
     a.contains(b.start) || a.contains(b.end) || b.contains(a.start) || b.contains(a.end)
}

val answer2 = assignments.grouped(2).count{ case Seq(a, b) => overlap(a, b)}
// 841