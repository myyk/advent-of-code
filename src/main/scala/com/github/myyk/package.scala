package com.github.myyk

import scala.io.Source
import scala.util.Using

def readInput(year: Int, day: Int): Seq[String] = {
  Using(Source.fromResource(s"samples/$year/day$day.txt")) { source => source.getLines.toSeq }.get
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
