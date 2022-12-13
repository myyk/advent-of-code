import com.github.myyk._

val input = readInput(2022,13)

val testInput = readStringInput("""[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]""")

case class Signal(list: List[Int|Signal])

object Signal {
    def apply(str: String): Signal = {
        ???
    }
}

def inputToSignalPairs(input: Iterable[String]): Iterable[Iterable[Signal]] = {
    val signals = for {
        line <- input
        if line.nonEmpty
    } yield {
        Signal(line)
    }

    signals.grouped(2).toVector
}

def isSignalInRightOrder(left: Signal, right: Signal): Boolean = {
    ???
}

def sumOfRightOrderedIndicies(signals: Iterable[Iterable[Signal]]): Int = {
    val rightOrderedIndicies = for {
        (Seq(left, right), i) <- signals.map(_.toSeq).zipWithIndex
        if isSignalInRightOrder(left, right)
    } yield {
        i+1
    }
    rightOrderedIndicies.sum
}

val answer1 = 1

val answer2 = 2