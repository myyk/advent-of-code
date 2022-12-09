import com.github.myyk._

val input = readInput(2022,9)

val testInput = readStringInput("""
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2""")

val testInput2 = readStringInput("""
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20""")

case class Instruction(direction: Direction, amount: Int)

object Instructions {
    def expand(instructions: Iterable[Instruction]): Iterable[Direction] = {
        for {
            Instruction(direction, amount) <- instructions
            _ <- (0 until amount)
        } yield {
            direction
        }
    }
}

def parseInput(input: Iterable[String]): Iterable[Instruction] = {
    for {
        line <- input
    } yield {
        val splits = line.split(" ").toSeq
        Instruction(Direction(splits(0).charAt(0)), splits(1).toInt)
    }
}

val instructions = parseInput(input)
val testInstructions = parseInput(testInput)
val testInstructions2 = parseInput(testInput2)

// test this workse for my input file
assert(instructions.toIndexedSeq(1110) == Instruction(Up, 11))
assert(instructions.last == Instruction(Left, 3))
assert(instructions.size == 2000)

assert((Point(0,0).diagonals intersect Point(1,2).adjacents).head == Point(1,1))


// points are head to tail
case class Rope(points: Seq[Point]) {
    def move(direction: Direction): Rope = {
        val rope = points.foldLeft(Seq.empty[Point]) { (rope, next) =>
            val nextMoved = if (rope.isEmpty) {
                next.move(direction)
            } else {
                val last = rope.last
                if ((last numMovesAwayIncludingDiagonals next) > 1) {
                    val movedSet = if (next.x == last.x || next.y == last.y) {
                        // if same row or col
                        (next.adjacents intersect last.adjacents)
                    } else {
                        // if diagonal
                        (next.diagonals intersect (last.adjacents union last.diagonals))
                    }
                    if (movedSet.size != 1) throw new Exception(s"old=${points}\n last = $last, next = $next, i = ${rope.size}\n$rope")
                    movedSet.head
                } else {
                    next
                }
            }
            rope.appended(nextMoved)
        }
        Rope(rope)
    }
}

object Rope {
    def apply(numPoints: Int): Rope = {
        val origin = Point(0,0)
        Rope(Seq.fill(numPoints)(origin))
    }
}

assert((Point(0,2) numMovesAwayIncludingDiagonals Point(0,0)) == 2)
assert((Point(1,1) numMovesAwayIncludingDiagonals Point(0,0)) == 1)
assert((Point(3,1) numMovesAwayIncludingDiagonals Point(0,0)) == 3)

def findWhereTailVisited(rope: Rope, instructions: Iterable[Instruction]): Seq[Point] = {
    val directions = Instructions.expand(instructions)
    val (finalRope, tailVisited) = directions.foldLeft((rope, Seq(rope.points.last))) { case ((rope, visited), direction) =>
        val nextRope = rope.move(direction)
        (nextRope, visited appended nextRope.points.last)
    }
    println(finalRope)
    tailVisited
}

def showMovement(points: Set[Point]): String = {
    val ys = points.map(_.y)
    val xs = points.map(_.x)
    val lines = for {
        y <- ys.min to ys.max
    } yield {
        val line = for {
            x <- xs.min to xs.max
        } yield {
            if (points.contains(Point(x,y))) {
                '#'
            } else {
                '.'
            }
        }
        line.mkString
    }
    lines.reverse.prepended("\n.").mkString("\n")
}

val testTailVisited = findWhereTailVisited(Rope(2), testInstructions)
testTailVisited.toSet.size
assert(testTailVisited.toSet.size == 13)

showMovement(testTailVisited.toSet)

val tailVisited = findWhereTailVisited(Rope(2), instructions)
showMovement(tailVisited.toSet)

val answer1 = tailVisited.toSet.size
// 6090

val testTailVisited3 = findWhereTailVisited(Rope(10), testInstructions2)
testTailVisited3.toSet.size
showMovement(testTailVisited3.toSet)
assert(testTailVisited3.toSet.size == 36)

val tailVisited2 = findWhereTailVisited(Rope(10), instructions)
val answer2 = tailVisited2.toSet.size
// 2566