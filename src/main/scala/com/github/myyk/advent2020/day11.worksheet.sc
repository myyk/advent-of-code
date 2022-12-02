import com.github.myyk._

val input = com.github.myyk.readInput(2020,11).toVector

sealed trait Space
object Floor extends Space
object OccupiedSeat extends Space
object EmptySeat extends Space

var abandonSeatThreshold = 4

val inputSeatingChart = for {
  row <- input
} yield {
  for {
    seat <- row.toCharArray.toVector
  } yield {
    seat match {
      case '.' => Floor
      case 'L' => EmptySeat
    }
  }
}

// Oops, this is overkill, but would be nice if needed later
case class AdjacentSeats(seats: Int, occupied:Int)

def adjacentSeats(row: Int, col: Int, seatingChart: Vector[Vector[Space]]): AdjacentSeats = {
  val adjacentSeats = for {
    r <- row-1 to row+1
    c <- col-1 to col+1
    if !(r==row && c==col)
    if r>=0 && r<seatingChart.length
    if c>=0 && c<seatingChart(0).length
  } yield {
    (r,c)
  }

  val occupied = adjacentSeats.count{ case (r,c) =>
    seatingChart(r)(c) == OccupiedSeat
  }
  AdjacentSeats(adjacentSeats.size, occupied)
}

def step(seatingChart: Vector[Vector[Space]], adjacentSeats: (Int, Int, Vector[Vector[Space]]) =>AdjacentSeats, abandonSeatThreshold:Int): Vector[Vector[Space]] = {
  for {
    (row, r) <- seatingChart.zipWithIndex
  } yield {
    for {
      (space, c) <- row.zipWithIndex
    } yield {
      space match {
        case EmptySeat if adjacentSeats(r, c, seatingChart).occupied == 0 =>
          OccupiedSeat
        case OccupiedSeat if adjacentSeats(r, c, seatingChart).occupied >= abandonSeatThreshold =>
          EmptySeat
        case noChange => noChange
      }
    }
  }
}

def printSeatingChart(seatingChart: Vector[Vector[Space]]):Unit = {
  for {
    row <- seatingChart
  } {
    for {
      space <- row
    } {
      space match {
        case EmptySeat => print("L")
        case OccupiedSeat => print("#")
        case Floor => print(".")
      }
    }
    println()
  }
}

def loop(seatingChart: Vector[Vector[Space]], adjacentSeats: (Int, Int, Vector[Vector[Space]]) =>AdjacentSeats, abandonSeatThreshold:Int): Vector[Vector[Space]] = {
  println("=== Initial State at i=0 ===")
  printSeatingChart(inputSeatingChart)

  var prev = inputSeatingChart
  var next = step(prev, adjacentSeats, abandonSeatThreshold)
  var i = 1

  while (prev != next) {
    println(s"=== State at i=$i === occupiedSeats=${next.map(_.count(_ == OccupiedSeat)).sum}")
    printSeatingChart(next)

    prev = next
    next = step(prev, adjacentSeats, abandonSeatThreshold)
    i+=1
  }

  next
}

val finalSeatingChart = loop(inputSeatingChart, adjacentSeats, abandonSeatThreshold)

// Answer 1
val answer1 = finalSeatingChart.map(_.count(_ == OccupiedSeat)).sum
//2303

var abandonSeatThreshold = 5

// Brute force no memoization. Could probably improve with remember the result for a (row,col,direction)
def adjacentSeatDirections(row: Int, col: Int, seatingChart: Vector[Vector[Space]]): AdjacentSeats = {
  val directions = for {
    r <- -1 to 1
    c <- -1 to 1
    if !(r==0 && c==0)
  } yield {
    (r,c)
  }

  val adjacentSeats = for {
    direction <- directions
  } yield {
    var r = row + direction._1
    var c = col + direction._2
    var result: Option[Space] = None
    var continue = true
    while (r>=0 && r<seatingChart.size && c>=0 && c<seatingChart(0).size && continue) {
      val space = seatingChart(r)(c)
      if (space == OccupiedSeat || space == EmptySeat) {
        result = Some(space)
        continue = false
      }
      r += direction._1
      c += direction._2
    }
    result
  }

  val occupied = adjacentSeats.flatten.count{
    _ == OccupiedSeat
  }
  AdjacentSeats(adjacentSeats.size, occupied)
}

val finalSeatingChart = loop(inputSeatingChart, adjacentSeatDirections, abandonSeatThreshold)

// Answer 2
val answer2 = finalSeatingChart.map(_.count(_ == OccupiedSeat)).sum
//2057