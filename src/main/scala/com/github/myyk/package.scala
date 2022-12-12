package com.github.myyk

import scala.io.Source
import scala.util.Using

def readInput(year: Int, day: Int): Seq[String] = {
  Using(Source.fromResource(s"samples/$year/day$day.txt")) { source => source.getLines.toSeq }.get
}

// Usually from a test string using """. Double check that it is correct in the first and last entries.
// And might need something else if you need empty lines.
def readStringInput(str: String, keepEmpty: Boolean = false): Seq[String] = {
  if (keepEmpty) {
    str.split("\n").map(_.trim).toSeq
  } else {
    str.split("\n").map(_.trim).filter(_.nonEmpty).toSeq
  }
}

// Groups the lines that are non-empty and consecutive with each other.
def groupNonEmptyLines(lines: Seq[String]): Vector[Vector[String]] = {
  var groupedInput = Vector.empty[Vector[String]]
  var nextGroup = Vector.empty[String]
  for {
    nextLine <- lines
  } yield {
    if (nextLine.isEmpty) {
      groupedInput = groupedInput :+ nextGroup
      nextGroup = Vector.empty[String]
    } else {
      nextGroup = nextGroup :+ nextLine
    }
  }
  if (nextGroup.nonEmpty) {
    groupedInput = groupedInput :+ nextGroup
  }
  
  groupedInput
}

case class Point(x: Int, y: Int) {
  def adjacents: Set[Point] = {
    Set(Up, Down, Left, Right).map(move)
  }

  def diagonals: Set[Point] = {
    Set(
      Point(x-1, y-1),
      Point(x-1, y+1),
      Point(x+1, y-1),
      Point(x+1, y+1),
    )
  }

  def move(direction: Direction): Point = {
    direction match {
      case Up => copy(y=y+1)
      case Down => copy(y=y-1)
      case Left => copy(x=x-1)
      case Right => copy(x=x+1)
    }
  }

  // Calculates the moves away where all 8 directions around a point are 1 away.
  def numMovesAwayIncludingDiagonals(other: Point): Int =  {
    val xDiff = (this.x - other.x).abs
    val yDiff = (this.y - other.y).abs
    xDiff max yDiff
  }
}

object Points {
  // provides lower left and upper right of a bounding box
  def boundaries(points: Iterable[Point]): (Point, Point) = {
    val pointSet = points.toSet
    val xs = pointSet.map(_.x)
    val ys = pointSet.map(_.y)
    
    (Point(xs.min, ys.min), Point(xs.max, ys.max))    
  }
}

sealed abstract trait Direction {
  val opposite: Direction
}
case object Up extends Direction {
  val opposite = Down
}
case object Down extends Direction {
  val opposite = Up
}
case object Left extends Direction {
  val opposite = Right
}
case object Right extends Direction {
  val opposite = Left
}

object Direction {
    def apply(char: Char): Direction = {
        char match {
            case 'U' => Up
            case 'D' => Down
            case 'L' => Left
            case 'R' => Right
        }
    }
}