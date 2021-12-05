package com.github.myyk.advent2021

import scala.io.Source
import scala.util.Using

val year = 2021

def readInput(day: Int):Seq[String] = {
  Using(Source.fromResource(s"samples/$year/day$day.txt")) { source => source.getLines.toSeq }.get
}
