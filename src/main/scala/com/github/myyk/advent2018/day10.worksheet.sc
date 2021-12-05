import com.github.myyk.advent2018._

//val fileSource = false
val fileSource = true
val rawInput = if (fileSource) {
  com.github.myyk.readInput(2018,10)
} else {
  Seq(
    "position=< 9,  1> velocity=< 0,  2>",
    "position=< 7,  0> velocity=<-1,  0>",
    "position=< 3, -2> velocity=<-1,  1>",
    "position=< 6, 10> velocity=<-2, -1>",
    "position=< 2, -4> velocity=< 2,  2>",
    "position=<-6, 10> velocity=< 2, -2>",
    "position=< 1,  8> velocity=< 1, -1>",
    "position=< 1,  7> velocity=< 1,  0>",
    "position=<-3, 11> velocity=< 1, -2>",
    "position=< 7,  6> velocity=<-1, -1>",
    "position=<-2,  3> velocity=< 1,  0>",
    "position=<-4,  3> velocity=< 2,  0>",
    "position=<10, -3> velocity=<-1,  1>",
    "position=< 5, 11> velocity=< 1, -2>",
    "position=< 4,  7> velocity=< 0, -1>",
    "position=< 8, -2> velocity=< 0,  1>",
    "position=<15,  0> velocity=<-2,  0>",
    "position=< 1,  6> velocity=< 1,  0>",
    "position=< 8,  9> velocity=< 0, -1>",
    "position=< 3,  3> velocity=<-1,  1>",
    "position=< 0,  5> velocity=< 0, -1>",
    "position=<-2,  2> velocity=< 2,  0>",
    "position=< 5, -2> velocity=< 1,  2>",
    "position=< 1,  4> velocity=< 2,  1>",
    "position=<-2,  7> velocity=< 2, -2>",
    "position=< 3,  6> velocity=<-1, -1>",
    "position=< 5,  0> velocity=< 1,  0>",
    "position=<-6,  0> velocity=< 2,  0>",
    "position=< 5,  9> velocity=< 1, -2>",
    "position=<14,  7> velocity=<-2,  0>",
    "position=<-3,  6> velocity=< 2, -1>"
  )
}

val entry = raw"position=<(\s*-?\d+),(\s*-?\d+)> velocity=<(\s*-?\d+),(\s*-?\d+)>".r

case class XYPair(x:Int, y:Int) {
  def +(other: XYPair): XYPair = {
    XYPair(this.x + other.x, this.y+other.y)
  }

  def distance(other: XYPair): Double = {
    Math.sqrt(Math.pow(other.x - this.x, 2) + Math.pow(other.y - this.y, 2))
  }
}

var lightsAndVelocities = for {
  next <- rawInput
} yield {
  next match {
    case entry(posX, posY, velX, velY) =>
      (XYPair(posX.trim.toInt,posY.trim.toInt), XYPair(velX.trim.toInt, velY.trim.toInt))
  }
}

def printLights(lights: Set[XYPair]): Unit = {
  for {
    y <- -100 to 100
    x <- -100 to 100
  } {
    if (x == -100) {
      println("")
    }

    val representation = if (lights.contains(XYPair(x,y))) {
      '#'
    } else {
      '.'
    }

    print(representation)
  }
}

def printLights2(center: XYPair, lights: Set[XYPair]): Unit = {
  for {
    y <- (center.y + -50) to (center.y + 50)
    x <- (center.x + -50) to (center.x + 50)
  } {
    if (x == (center.x + -50)) {
      println("")
    }

    val representation = if (lights.contains(XYPair(x,y))) {
      '#'
    } else {
      '.'
    }

    print(representation)
  }
}

def average(lights: Set[XYPair]): XYPair = {
  val xAvg = lights.toList.map(_.x).sum / lights.size
  val yAvg = lights.toList.map(_.y).sum / lights.size

  XYPair(xAvg, yAvg)
}

def maxDistance(point: XYPair, lights: Set[XYPair]): Double = {
  lights.map(_.distance(point)).max
}

for {i <- 1 to 100000} {
  lightsAndVelocities = for {
    (lights, velocities) <- lightsAndVelocities
  } yield {
    (lights + velocities, velocities)
  }

  val currentLights = lightsAndVelocities.toMap.keySet
  val avg = average(currentLights)
  val furthestDistance = maxDistance(avg, currentLights)

//  if (currentLights.filter{xy => xy.x > -300 && xy.x < 300 && xy.y > -300 && xy.y < 300}.size > 10) {
  if (furthestDistance < 100) {
//  if (i == 10418) {
    println(i)
    printLights2(avg, currentLights)
  }
}

// Answer 1
val ans1 = "ZNNRZJXP"

// Answer 2
val ans2 = 10418