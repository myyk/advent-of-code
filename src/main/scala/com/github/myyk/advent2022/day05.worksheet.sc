import com.github.myyk._

val input = readInput(2022,5)

val (stackDefinition, stackDefIndex) = input.zipWithIndex.find(_._1.startsWith(" 1")).get
val numStacks = stackDefinition.trim.split(" ").map(_.trim).filter(_.nonEmpty).size

val stackLines = input.take(stackDefIndex)
// Double check this looks correct
stackLines.last

// These are top to bottom in each stack

case class Stacks[T](stacks: Vector[List[T]]) {
    def push(stack: Int, value: T): Stacks[T] = {
        Stacks(stacks.updated(stack, value :: stacks(stack)))
    }

    def pop(stack: Int): (T, Stacks[T]) = {
        (stacks(stack).head, Stacks(stacks.updated(stack, stacks(stack).tail)))
    }

    // maybe not needed for this problem, implemented by accident but working
    def move(amount: Int, from: Int, to: Int): Stacks[T] = {
        // pop from amount times
        var stacks = this
        var acc = List.empty[T]
        for (i <- 0 until amount) {
            val (next, nextStacks) = stacks.pop(from)
            acc = next :: acc
            stacks = nextStacks
        }

        // push to with acc
        while (acc.nonEmpty) {
            stacks = stacks.push(to, acc.head)
            acc = acc.tail
        }

        stacks
    }

    // push then pop between each moving, so ends up flipped
    def flipMove(amount: Int, from: Int, to: Int): Stacks[T] = {
        // pop from the one and pop to the other amount times
        (0 until amount).foldRight(this)((_, stacks) => {
            val (next, nextStacks) = stacks.pop(from)
            nextStacks.push(to, next)
        })
    }

    def readTops: String = {
        stacks.map(_.head).mkString
    }
}

object Stacks {
    def apply[T](n: Int): Stacks[T] = {
        new Stacks(Vector.fill(n)(List.empty[T]))
    }
}

var testStacks = Stacks[Char](2)
testStacks = testStacks.push(0, 'B')
testStacks = testStacks.push(0, 'A')
testStacks = testStacks.push(1, 'D')
testStacks = testStacks.push(1, 'C')
testStacks

testStacks.move(2, 0, 1)


// reverse stack lines parse them and add them onto the stacks
// val crateRegex = raw" [(.*)]".r
val crateRegex = " \\[(\\w)\\]".r

// test that regex is able to parse a crate
" [f]" match {
    case crateRegex(crate) => crate.toCharArray().head
    case _ => ' '
}

def createInputStacks(): Stacks[Char] = {
    var stacks = Stacks[Char](numStacks)
    for {
        stackLine <- stackLines.reverse
        (crateDef, stack) <- (" " + stackLine).grouped(4).zipWithIndex
        case crateRegex(crateStr) <- Some(crateDef)
    } {
        assert(crateStr.size == 1)
        val crate = crateStr.head

        stacks = stacks.push(stack, crate)
    }
    stacks
}

// check to see this looks like reading all the tops of stacks, since this is how we read the answer
createInputStacks().readTops

val proceduresLines = input.takeRight(input.length - stackDefIndex - 2)

// check that this looks right
proceduresLines.head
proceduresLines.last

case class Procedure(amount: Int, from: Int, to: Int)

val procedureRegex = "move (\\d+) from (\\d+) to (\\d+)".r
val procedures = for {
    proceduresLine <- proceduresLines
    case procedureRegex(amount, from, to) <- Some(proceduresLine)
} yield {
    Procedure(amount.toInt, from.toInt, to.toInt)
}

var stacks9000 = createInputStacks()
for {
    Procedure(amount, from, to) <- procedures
} {
    stacks9000 = stacks9000.flipMove(amount, from-1, to-1)
}

stacks9000

val answer1 = stacks9000.readTops
// ZSQVCCJLL

var stacks9001 = createInputStacks()
for {
    Procedure(amount, from, to) <- procedures
} {
    stacks9001 = stacks9001.move(amount, from-1, to-1)
}

stacks9001

val answer2 = stacks9001.readTops
// QZFJRWHGS