import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(20205)

// Row - Convert to binary in order and then just use string binary to int.
val binaryMapping= Map(
  "F" -> "0",
  "B" -> "1",
  "R" -> "1",
  "L" -> "0"
)

val rowMultiplier = 8

case class TicketInfo(row:Int, column:Int) {
  def seatID: Int = {
    row * rowMultiplier + column
  }
}

val ticketInfo = for {
  next <- input
} yield {
  val binaryString = binaryMapping.foldLeft(next){case (last, (from, to)) =>last.replace(from, to)}
  val (row, col) = binaryString.splitAt(7)
  TicketInfo(
    row = Integer.parseInt(row, 2),
    column = Integer.parseInt(col, 2),
  )
}

val seatIDs = ticketInfo.map(_.seatID).toSet

// Answer 1
val answer1 = seatIDs.max

val min = seatIDs.min
val max = seatIDs.max

val missingSeatID = (min to max).find(!seatIDs.contains(_))

// Answer 2
val answer2 = missingSeatID.get