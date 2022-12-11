import com.github.myyk._

val input = readInput(2022,10)

val testInput = readStringInput("""
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
""")

val simpleTestInput = readStringInput("""
noop
addx 3
addx -5
""")

sealed trait Instruction {
    def cycles: Int
}
case object Noop extends Instruction {
    val cycles: Int = 1
}
case class AddX(toAdd: Int) extends Instruction {
    val cycles: Int = 2
}

def inputToInstructions(input: Seq[String]): Iterable[Instruction] = {
    for {
        line <- input
    } yield {
        line match {
            case "noop" =>
                Noop
            case addX => AddX(addX.replace("addx", "").trim.toInt)
        }
    }
}

val testInstructions = inputToInstructions(testInput)
val testSimpleInstructions = inputToInstructions(simpleTestInput)
val instructions = inputToInstructions(input)

def xRegisterHistory(instructions: Iterable[Instruction]): IndexedSeq[Int] = {
    val cycleHistory = instructions.foldLeft(List(1)) { case (xHistory, instruction) =>
        val nextX = instruction match {
            case Noop => xHistory.head
            case AddX(toAdd) => xHistory.head + toAdd
        }
        // add nextX and n-1 cycles of the same value
        nextX :: (1 until instruction.cycles).foldLeft(xHistory) {case (xHistory, _) =>
            xHistory.head :: xHistory
        }
    }

    cycleHistory.reverse.toVector
}

def signalStrength(signalHistory: IndexedSeq[Int], moment: Int): Int = {
    signalHistory(moment-1) * moment
}

xRegisterHistory(testSimpleInstructions)

val testHistoryX = xRegisterHistory(testInstructions)
assert(testHistoryX(20-1) == 21)
assert(testHistoryX(60-1) == 19)
assert(testHistoryX(100-1) == 18)
assert(testHistoryX(140-1) == 21)
assert(testHistoryX(180-1) == 16)
assert(testHistoryX(220-1) == 18)

assert(signalStrength(testHistoryX, 20) == 420)
assert(signalStrength(testHistoryX, 220) == 3960)

val historyX = xRegisterHistory(instructions)

val moments = Seq(20,60,100,140,180,220)

val answer1 = moments.map(signalStrength(historyX, _)).sum
// 13520

// You count the pixels on the CRT: 40 wide and 6 high.
// This CRT screen draws the top row of pixels left-to-right, 
// then the row below that, and so on. The left-most pixel in
// each row is in position 0, and the right-most pixel in each
// row is in position 39.

def spriteVisibleLocations(xRegister: Int): Set[Int] = {
    Set(xRegister-1, xRegister, xRegister+1)
}

def consoleOut(historyX: Iterable[Int]): String = {
    val output = for {
        (spriteLocation, i) <- historyX.zipWithIndex
    } yield {
        val crtPosition = i%40
        if (spriteVisibleLocations(spriteLocation).contains(crtPosition)) {
            '#'
        } else {
            '.'
        }

    }
    output.grouped(40).map(_.mkString).mkString("\n")
}

consoleOut(testHistoryX)

val answer2 = consoleOut(historyX)
/*
###...##..###..#..#.###..####..##..###..
#..#.#..#.#..#.#..#.#..#.#....#..#.#..#.
#..#.#....#..#.####.###..###..#..#.###..
###..#.##.###..#..#.#..#.#....####.#..#.
#....#..#.#....#..#.#..#.#....#..#.#..#.
#.....###.#....#..#.###..####.#..#.###..
*/
// PGPHBEAB