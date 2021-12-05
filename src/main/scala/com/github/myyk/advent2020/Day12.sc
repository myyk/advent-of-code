import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(202012)

// assuming north is positive, south is negative.
// assuming east is positive, west is negative.
// ship starts facing east

sealed trait NavigationInstruction
class Direction(val north:Int, val east:Int, val degreesToLeft:Int) extends NavigationInstruction {
  def +(other:Direction) : Direction = {
    new Direction(this.north + other.north, this.east + other.east, degreesToLeft)
  }
  def *(value: Int) : Direction = {
    new Direction(this.north * value, this.east * value, degreesToLeft)
  }
  def manhattanDistance:Int = {
    this.north.abs + this.east.abs
  }
}
case class North(value:Int) extends Direction(value, 0, 90)
case class South(value:Int) extends Direction(-value, 0, 270)
case class East(value:Int) extends Direction(0, value, 0)
case class West(value:Int) extends Direction(0, -value, 180)

val leftDirections = Vector(East(1), North(1), West(1), South(1))

case class LeftTurn(degrees:Int) extends NavigationInstruction {
  def turn(current: Direction): Direction = {
    val shiftsLeftOfEast = ((current.degreesToLeft + degrees)/90%4+4)%4
    leftDirections(shiftsLeftOfEast)
  }
}

case class Forward(value:Int) extends NavigationInstruction

val navInstructionPattern = "([\\w])([\\d]+)".r

val navInstructions = for {
  next <- input
} yield {
  next match {
    case navInstructionPattern(instruction, value) =>
      val intValue = value.toInt
      instruction match {
        case "N" => North(intValue)
        case "S" => South(intValue)
        case "E" => East(intValue)
        case "W" => West(intValue)
        case "L" => LeftTurn(intValue)
        case "R" => LeftTurn(-intValue)
        case "F" => Forward(intValue)
      }
  }
}

// Answer 1
val answer1 = {
  // TODO: If modeled better the ship would be a thing and it would take a TurnLeft to turn.
  var shipDirection: Direction = East(1)
  var shipPosition: Direction = East(0)
  for {
    next <- navInstructions
  } {
    next match {
      case direction: Direction =>
        shipPosition = shipPosition + direction
      case leftTurn: LeftTurn =>
        shipDirection = leftTurn.turn(shipDirection)
      case Forward(value) =>
        shipPosition = shipPosition + (shipDirection * value)
    }
  }
  shipPosition.manhattanDistance
}
// 1441

// Answer 2
val answer2 = {
  var waypointPosition: Direction = East(10) + North(1)
  var shipPosition: Direction = East(0)
  for {
    next <- navInstructions
  } {
    next match {
      case direction: Direction =>
        waypointPosition = waypointPosition + direction
      case leftTurn: LeftTurn =>
        waypointPosition =
          (leftTurn.turn(North(1))*waypointPosition.north) +
            (leftTurn.turn(East(1))*waypointPosition.east)
      case Forward(value) =>
        shipPosition = shipPosition + (waypointPosition * value)
    }
  }
  shipPosition.manhattanDistance
}
//61616