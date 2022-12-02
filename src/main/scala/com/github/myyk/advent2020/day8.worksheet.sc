import com.github.myyk._

val input = com.github.myyk.readInput(2020,8).toVector

sealed trait Command
case class Noop(value: Int) extends Command
case class Jmp(value: Int) extends Command
case class Acc(value: Int) extends Command

val nop = "nop ([+-][\\d]+)".r
val jmp = "jmp ([+-][\\d]+)".r
val acc = "acc ([+-][\\d]+)".r

val commands = for {
  next <- input
} yield {
  next match {
    case nop(value) => Noop(value.toInt)
    case jmp(value) => Jmp(value.toInt)
    case acc(value) => Acc(value.toInt)
  }
}

case class Result(acc:Int, ranToEnd: Boolean)

def runCommands(commands: Vector[Command]): Result = {
  var seenCommands = Set.empty[Int]
  var i = 0
  var accumulator = 0
  while (!seenCommands.contains(i) && i < commands.size) {
    seenCommands += i
    val nextCommand = commands(i)

    nextCommand match {
      case Noop(_) => i += 1
      case Acc(value) =>
        accumulator += value
        i += 1
      case Jmp(value) =>
        i += value
    }
  }

  Result(accumulator,i == commands.size)
}

// Answer 1
val answer1 = runCommands(commands).acc
//1584

// I think this part is faster to just bruteforce bc there aren't that many options and it's exectution is something like O(n^2) which isn't so big
val results = for {
  (cmd, i) <- commands.zipWithIndex.view
  if !cmd.isInstanceOf[Acc]
} yield {

  val newCmd = cmd match {
    case Noop(value) => Jmp(value)
    case Jmp(value) => Noop(value)
    case _ => ???
  }
  val newCommands = commands.updated(i,newCmd)
  runCommands(newCommands)
}

val result = results.find(_.ranToEnd)

// Answer 2
val answer2 = result.get.acc
//920