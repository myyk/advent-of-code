import com.github.myyk._

val input = readInput(2021,2)

// Part 1

// Instructions are R,L,U,D
sealed trait Instruction
case object R extends Instruction
case object U extends Instruction
case object D extends Instruction

// Construct Instruction from Strings where "forward" is R, "down" is D, "up" is U
object Instruction {
  def apply(s: String): Instruction = s match {
    case "forward" => R
    case "down" => D
    case "up" => U
  }
}


// Direction is an instruction and a value
case class Direction(instruction: Instruction, value: Int)

// split inputs on space then map to directions
object Direction {
  def apply(s: String): Direction = {
    val split = s.split(" ")
    Direction(Instruction(split(0)), split(1).toInt)
  }
}


val directions = input.map(Direction(_))

// depth and horizontal position start at 0
var depth = 0
var horizontalPosition = 0

// loop through directions and move the position. D should increase depth, U should decrease depth, R should increase horizontal position, L should decrease horizontal position
directions.foreach {
  case Direction(R, value) => horizontalPosition += value
  case Direction(D, value) => depth += value
  case Direction(U, value) => depth -= value
}

// print depth and horizontal position for debugging
depth
horizontalPosition

// What do you get if you multiply your final horizontal position by your final depth?
val answer1 = horizontalPosition * depth

// Notes: That code is kind of shit and it's it should be refactored. Copilot authored most of that other than the comments.

// Track aim starting at 0
var aim = 0

// reset depth and horizontal position
depth = 0
horizontalPosition = 0

// Loop through directions and move the aim. D should increase aim, U should decrease aim. R should increase horizontal position and increase depth multiplied by aim
directions.foreach {
  case Direction(R, value) => {
    horizontalPosition += value
    depth += aim * value
  }
  case Direction(D, value) => aim += value
  case Direction(U, value) => aim -= value
}

depth
horizontalPosition

val answer2 = horizontalPosition * depth
