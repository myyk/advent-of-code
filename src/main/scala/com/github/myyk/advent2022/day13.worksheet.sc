import com.github.myyk._

import scala.annotation.tailrec

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

case class Signal(list: Vector[Int|Signal] = Vector.empty) {
    def appended(el: Int|Signal): Signal = {
        copy(list=list :+ el)
    }

    def pop: (Option[Int|Signal], Signal) = {
        this.list.headOption match {
            case Some(head) => (Some(head), copy(list=list.tail))
            case None => (None, this)
        }
    }

    @`inline` final def :+ (x: Int|Signal) = appended(x)

    def size: Int = list.size

    def headOption: Option[Int] = list.headOption match {
        case Some(signal: Signal) => signal.headOption
        case Some(head: Int) => Some(head)
        case None => None
    }
}

object Signal {
    def apply(str: String): Signal = {
        val encodedStr = str.replace("10", "A")

        var lists = Vector.empty[Signal]
        var current: Signal = Signal()
        for {
            next <- encodedStr.toCharArray
        } {
            next match {
                case '[' =>
                    lists = current +: lists
                    current = Signal()
                case ']' =>
                    current = lists.head :+ current
                    lists = lists.tail
                case 'A' =>
                    current = current :+ 10
                case ',' =>
                case num =>
                    current = current :+ num.asDigit
            }
        }
        current.list.head.asInstanceOf[Signal]
    }
}

Signal("[1,[2,[3,[4,[5,6,10]]]],8,9]")

Signal("[]").pop
Signal("[1,2,[3]]").pop
Signal(Vector(1)).pop

def inputToSignalPairs(input: Iterable[String]): Iterable[(Signal, Signal)] = {
    val signals = for {
        line <- input
        if line.nonEmpty
    } yield {
        Signal(line)
    }

    for {
        Seq(left, right) <- signals.grouped(2).map(_.toSeq).toVector
    } yield {
        (left, right)
    }
}

final def isSignalInRightOrder(toCheck: List[(Signal,Signal)]): Boolean = {
    toCheck.isEmpty || {
        val (left, right) = toCheck.head

        val (leftHeadOption, leftTail) = left.pop
        val (rightHeadOption, rightTail) = right.pop

        (leftHeadOption, rightHeadOption) match {
            case (None, None) =>
                isSignalInRightOrder(toCheck.tail)
            case (Some(leftEl: Int), Some(rightEl: Int)) => 
                (leftEl <= rightEl) && isSignalInRightOrder((leftTail, rightTail) :: toCheck.tail)
            case (Some(leftEl: Signal), Some(rightEl: Signal)) =>
                leftEl.list.isEmpty || isSignalInRightOrder((leftEl, rightEl) :: (leftTail, rightTail) :: toCheck.tail)


            case (Some(el), None) => false
            case (None, Some(el)) => isSignalInRightOrder(toCheck.tail)

            case (Some(leftEl: Int), Some(rightEl: Signal)) =>
                rightEl.headOption.nonEmpty && {
                    (leftEl <= rightEl.headOption.get) && isSignalInRightOrder(toCheck.tail)
                }
            case (Some(leftEl: Signal), Some(rightEl: Int)) =>
                leftEl.headOption.isEmpty || {
                    (leftEl.headOption.get <= rightEl) && isSignalInRightOrder(toCheck.tail)
                }
            // case other => throw new Exception(other.toString)
        }
    }
}


// final def isSignalInRightOrder(toCheck: List[(Signal,Signal)]): Boolean = {
//     isSignalInRightOrder0(toCheck).isEmpty
// }

// // Has a string means false for the reason
// @tailrec
// final def isSignalInRightOrder0(toCheck: List[(Signal,Signal)]): Option[String] = {
//     if (toCheck.isEmpty) {
//         None
//     } else {
//         val (left, right) = toCheck.head

//         val (leftHeadOption, leftTail) = left.pop
//         val (rightHeadOption, rightTail) = right.pop
//         (leftHeadOption, rightHeadOption) match {
//             case (None, None) =>
//                 isSignalInRightOrder0(toCheck.tail)
//             case (Some(el), None) => Some(s"$el on left, none on right")
//             case (None, Some(el)) =>
//                 isSignalInRightOrder0(toCheck.tail)

//             case (Some(leftEl: Int), Some(rightEl: Signal)) =>
//                 rightEl.headOption match {
//                     case None => Some("TODO")
//                     case Some(rightEl) => (leftEl <= rightEl) && isSignalInRightOrder0(toCheck.tail)
//                 }
//             case (Some(leftEl: Signal), Some(rightEl: Int)) =>
//                 leftEl.headOption match {
//                     case None => isSignalInRightOrder0(toCheck.tail)
//                     case Some(leftEl) => (leftEl <= rightEl) && isSignalInRightOrder0(toCheck.tail)
//                 }
//             case (Some(leftEl: Int), Some(rightEl: Int)) => 
//                 if (leftEl <= rightEl) {println(s"(leftEl <= rightEl): $leftEl, $rightEl, ${(leftTail, rightTail)} ")}
//                 (leftEl <= rightEl) && isSignalInRightOrder0((leftTail, rightTail) :: toCheck.tail)

//             case (Some(leftEl: Signal), Some(rightEl: Signal)) =>
//                 leftEl.list.isEmpty || isSignalInRightOrder0((leftEl, rightEl) :: (leftTail, rightTail) :: toCheck.tail)
//             // case other => throw new Exception(other.toString)
//         }
//     }
// }

def testIsSignalInRightOrder(left: Signal, right: Signal, expected: Boolean): Unit = {
    assert(isSignalInRightOrder(List((left, right))) == expected)
}

testIsSignalInRightOrder(Signal("[]"), Signal("[]"), true)
testIsSignalInRightOrder(Signal("[1]"), Signal("[1]"), true)
testIsSignalInRightOrder(Signal("[1]"), Signal("[]"), false)
testIsSignalInRightOrder(Signal("[]"), Signal("[1]"), true)
val (left, leftTail) = Signal("[[1]]").pop
val (right, rightTail) = Signal("[1]").pop
(left, right)
(leftTail, rightTail)
Signal(Vector.empty).pop
testIsSignalInRightOrder(Signal("[[1]]"), Signal("[1]"), true)
testIsSignalInRightOrder(Signal("[1]"), Signal("[[1]]"), true)
testIsSignalInRightOrder(Signal("[9]"), Signal("[[8,7,6]]"), false)
testIsSignalInRightOrder(Signal("[[2,3,4]]"), Signal(Vector(4)), true)
testIsSignalInRightOrder(Signal("[[2,3,4]]"), Signal("[[4]]"), false)
testIsSignalInRightOrder(Signal("[[1],[2,3,4]]"), Signal("[[1],4]"), true)

def sumOfRightOrderedIndicies(signals: Iterable[(Signal, Signal)]): Int = {
    val rightOrderedIndicies = for {
        ((left, right), i) <- signals.zipWithIndex
        if isSignalInRightOrder(List((left, right)))
    } yield {
        i+1
    }
    rightOrderedIndicies.sum
}

def solve1(input: Seq[String]): Int = {
    val signalPairs = inputToSignalPairs(input)
    sumOfRightOrderedIndicies(signalPairs)
}

val testSignalPairs = inputToSignalPairs(testInput).toVector
testSignalPairs.map(n => isSignalInRightOrder(List(n))) == Vector(true,true,false,true,false,true,false,false)

val testAnswer1 = solve1(testInput)

val answer1 = solve1(input)
// 2522 - Too Low

val answer2 = 2