package com.github.myyk

import scala.io.Source
import scala.util.Using

// In Intellij, you must build the project before this can be used from the worksheets.

package object advent2018 {
  val projectBase = "/home/myyk/workspace/advent-of-code"
  val sampleDir = projectBase + "/samples/2018"

  def readInput(day: Int):Seq[String] = {
    Using(Source.fromFile(s"$sampleDir/day$day.txt")) { source => source.getLines().toSeq }.get
  }
}
