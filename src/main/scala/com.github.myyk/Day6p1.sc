import scala.annotation.tailrec
import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

case class Coor(x:Int, y:Int, value: Char = '.')
case class Point(value: Char = '.', distance: Int = Int.MaxValue)

val fileSource = true
//val fileSource = false
val rawInputs = if (fileSource) {
  for {
    next <- readInput(6)
  } yield  {
    val splits = next.split(",")
    Coor(splits(0).toInt, splits(1).trim.toInt)
  }
} else {
  List(
    Coor(1, 1),
    Coor(1, 6),
    Coor(8, 3),
    Coor(3, 4),
    Coor(5, 5),
    Coor(8, 9),
  )
}

val inputs = rawInputs.zipWithIndex.map { case (next, i) =>
  Coor(next.x, next.y, ('a'.toInt+i).toChar)
}.toSet[Coor]

def findDimensions(): (Int, Int) = {
  val maxX = inputs.maxBy(_.x).x
  val maxY = inputs.maxBy(_.y).y
  return (maxX+2, maxY+2) // 1 for the 0s and 1 for the border
}

val (areaX, areaY) = findDimensions()

val grid = Array.fill(areaY)(Array.fill(areaX)(Point()))
def printGrid(): Unit = {
  for {
    (row, y) <- grid.zipWithIndex
  } {
    for {
      (next, x) <- row.zipWithIndex
    } {
      val toPrint = if (inputs.contains(Coor(x,y,next.value))) {
        next.value.toUpper
      } else {
        next.value
      }
      print(toPrint)
    }
    println()
  }
}

def getNeightbors(coor: Coor): Set[Coor] = {
  Set(
    coor.copy(x = coor.x - 1),
    coor.copy(x = coor.x + 1),
    coor.copy(y = coor.y - 1),
    coor.copy(y = coor.y + 1)
  ).filter{ case Coor(x, y, _) =>
      y >= 0 && y < grid.size && x >= 0 && x < grid.head.size
  }
}

@tailrec
def addAreaToGrid(toVisit:Set[Coor], distance:Int, visited: Set[Coor]): Unit = {
  if (toVisit.isEmpty) {
    return
  }

  val neighborBuilder = Set.newBuilder[Coor]
  for {
    next <- toVisit
  } yield {
    // update grid
    val gridPoint = grid(next.y)(next.x)
    grid(next.y)(next.x) = if (gridPoint.distance > distance) {
      // add neighbors
      neighborBuilder ++= getNeightbors(next)
      Point(next.value, distance)
    } else if (gridPoint.distance == distance) {
      // add neighbors
      neighborBuilder ++= getNeightbors(next)
      Point('.', distance)
    } else {
      gridPoint
    }
  }

  val toVisitNext = neighborBuilder.result() -- visited
  addAreaToGrid(toVisitNext, distance+1, visited ++ toVisitNext)
}

// Construct grid
println("Constructing grid")
for {
  next <- inputs
} {
  println(s"Adding $next")
  addAreaToGrid(Set(next), 0, Set.empty)
}

printGrid

// Get non-edges
val topAndBottomValues = for {
  row <- Set(grid.head, grid.last)
  next <- row
} yield {
  next.value
}
val leftAndRightValues = for {
  row <- grid
  next <- Set(row.head, row.last)
} yield {
  next.value
}
val infiniteValues = topAndBottomValues ++ leftAndRightValues

// Occurrances per value
val occurrances = grid.flatten.groupBy(_.value).map{ case (value, elems) => (value, elems.size)}
val nonInfiniteOccurances = occurrances.filterNot(n => infiniteValues.contains(n._1))

// Answer 1
val (ans1, occ)= nonInfiniteOccurances.maxBy(_._2)
//ans1 = 4829
