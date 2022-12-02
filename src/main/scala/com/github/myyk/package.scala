package com.github.myyk

import scala.io.Source
import scala.util.Using

def readInput(year: Int, day: Int):Seq[String] = {
  Using(Source.fromResource(s"samples/$year/day$day.txt")) { source => source.getLines.toSeq }.get
}
