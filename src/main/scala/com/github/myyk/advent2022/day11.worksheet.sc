import com.github.myyk._

val input = readInput(2022,11)

val testInput = readStringInput("""Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
""", keepEmpty = true)

type Worry = BigInt

case class Item(worry: Worry, recoveryDivisor: Int) {
    def inspect(operation: Operation): Item = {
        copy(operation.apply(worry)/recoveryDivisor)
    }
}

sealed abstract trait Operation {
    def apply(old: Worry): Worry
}
case class MultiplyByConstant(constant: Int) extends Operation {
    def apply(old: Worry): Worry = {
        old*constant
    }
}
case object MultiplyBySelf extends Operation {
    def apply(old: Worry): Worry = {
        old*old
    }
}
case class AddByConstant(constant: Int) extends Operation {
     def apply(old: Worry): Worry = {
        old+constant
    }
}

object Operation {
    def apply(opString: String): Operation = {
        val splits = opString.split(" ").map(_.trim)
        val op = splits(0).charAt(0)
        op match {
            case '*' => 
                splits(1) match {
                    case "old" => MultiplyBySelf
                    case constant => MultiplyByConstant(constant.toInt)
                }
            case '+' => AddByConstant(splits(1).toInt)
        }
    }
}

case class Monkey(id: Int, items: Vector[Item], operation: Operation, divisor: Int, trueMonkey: Int, falseMonkey: Int, inspections: Int = 0) {
    def takeTurn(monkeys: IndexedSeq[Monkey]): IndexedSeq[Monkey] = {
        val allDivisor = monkeys.map(_.divisor).product
        // inspect each item
        items.foldLeft(monkeys){ (monkeys, item) =>
            val inspectedItem = {
                val i = item.inspect(operation)
                // we can divide by mod by all divisors because future divisions by primes
                // which all divisors are unique primes, will still give the same signal
                i.copy(worry = i.worry%allDivisor)
            }

            val throwMonkey = monkeys(throwTo(inspectedItem))
            monkeys
                .updated(throwMonkey.id, throwMonkey.addItem(inspectedItem))
        }.updated(id, copy(items = Vector.empty, inspections = inspections + items.size))
    }

    def throwTo(item: Item): Int = {
        if (item.worry % divisor == 0) {
            trueMonkey
        } else {
            falseMonkey
        }
    }

    def addItem(item: Item): Monkey = {
        copy(items = items.appended(item))
    }
}

object Monkey {
    def apply(input: IndexedSeq[String]): Monkey = {
        val id = input(0).replace("Monkey","").replace(":","").trim.toInt
        val startingItemsWorry = input(1).replace("Starting items: ", "").trim.split(",").map(_.trim.toInt)
        val items = startingItemsWorry.map(Item(_, 3)).toVector
        val operation = Operation(input(2).replace("Operation: new = old", "").trim)
        val divisor = input(3).replace("Test: divisible by ", "").trim.toInt
        val trueMonkey = input(4).replace("If true: throw to monkey ", "").trim.toInt
        val falseMonkey = input(5).replace("If false: throw to monkey ", "").trim.toInt

        Monkey(id, items, operation, divisor, trueMonkey, falseMonkey)
    }
}

def parseMonkeys(input: Iterable[String]): IndexedSeq[Monkey] = {
    input.grouped(7).map(lines => Monkey(lines.toIndexedSeq)).toIndexedSeq
}

def changeRecoveryDivisor(recoveryDivisor: Int, monkeys: IndexedSeq[Monkey]): IndexedSeq[Monkey] = {
    for {
        monkey <- monkeys
    } yield {
        val items = for {
            item <- monkey.items
        } yield {
            item.copy(recoveryDivisor = recoveryDivisor)
        }
        monkey.copy(items = items)
    }
}

val startMonkeys = parseMonkeys(input)

val testStartMonkeys = parseMonkeys(testInput)

def takeRound(monkeys: IndexedSeq[Monkey]): IndexedSeq[Monkey] = {
    (monkeys.indices).foldLeft(monkeys) { (monkeys, i) =>
        monkeys(i).takeTurn(monkeys)
    }
}

def takeRounds(n: Int, monkeys: IndexedSeq[Monkey]): IndexedSeq[Monkey] = {
    (0 until n).foldLeft(monkeys) { (monkeys, _) =>
        takeRound(monkeys)
    }
}

def printMonkeyItems(monkeys: IndexedSeq[Monkey]): String = {
    (for {
        (monkey, i) <- monkeys.zipWithIndex
    } yield {
        s"Monkey $i: ${monkey.items.map(_.worry).mkString(",")}"
    }).mkString("\n")
}

printMonkeyItems(testStartMonkeys)
val testMonkeyState = takeRounds(20, testStartMonkeys)
assert(testMonkeyState(0).inspections==101)
assert(testMonkeyState(1).inspections==95)
assert(testMonkeyState(2).inspections==7)
assert(testMonkeyState(3).inspections==105)

def monkeyBusiness(monkeys: IndexedSeq[Monkey]): Long = {
    monkeys.map(_.inspections.toLong).sorted.reverse.take(2).product
}

assert(monkeyBusiness(testMonkeyState) == 10605)

val monkeyState = takeRounds(20, startMonkeys)

val answer1 = monkeyBusiness(monkeyState)
// 110264

def maximalWorry(monkeys: IndexedSeq[Monkey]): IndexedSeq[Monkey] = {
    changeRecoveryDivisor(1, monkeys)
}

val testStartMonkeys2 = maximalWorry(testStartMonkeys)
val testMonkeyState2 = takeRounds(10000, testStartMonkeys2)
testMonkeyState2.map(_.inspections)
assert(monkeyBusiness(testMonkeyState2) == 2713310158L)

val monkeyState2 = takeRounds(10000, maximalWorry(startMonkeys))

val answer2 = monkeyBusiness(monkeyState2)
// 23612457316
