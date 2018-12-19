import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

//val fileSource = false
val fileSource = true
val rawInput = if (fileSource) {
  readInput(13)
} else {
  Seq(
      """/->-\         """,
      """|   |  /----\ """,
      """| /-+--+-\  | """,
      """| | |  | v  | """,
      """\-+-/  \-+--/ """,
      """  \------/    """,
  )
}

// Extended from day13
case class XYPair(x:Int, y:Int) {
  def +(other: XYPair): XYPair = {
    XYPair(this.x + other.x, this.y+other.y)
  }

  def distance(other: XYPair): Double = {
    Math.sqrt(Math.pow(other.x - this.x, 2) + Math.pow(other.y - this.y, 2))
  }

  def ^(): XYPair = {
    this.copy(y = y-1)
  }

  def v(): XYPair = {
    this.copy(y = y+1)
  }

  def >(): XYPair = {
    this.copy(x = x+1)
  }

  def <(): XYPair = {
    this.copy(x = x-1)
  }
}

implicit val XYPairOrdering: Ordering[XYPair] =
  Ordering by (loc => (loc.x, loc.y))

case class Cart(direction: Char, intersectionNum: Int = 0, isWorking: Boolean = true) {
  // not the best data modeling, but this is throw away code
  def move(loc: XYPair, graph: Map[XYPair, Set[XYPair]], carts: Map[XYPair, Cart]): Map[XYPair, Cart] = {
    if (this.isWorking) {
      val cart = if (graph(loc).size == 4) {
        intersectionAdjusted
      } else {
        this
      }

      // move the cart
      val nextPoint = cart.nextLocationFrom(loc)

      if (carts.contains(nextPoint)) {
        // break carts
        // Assuming direction and # of cards here doesn't matter
        carts - loc + (nextPoint -> cart.break)
      } else {
        carts - loc + (nextPoint -> cart.withNextDirection(loc, nextPoint, graph))
      }
    } else {
      // don't move the cart
      carts
    }
  }

  def nextLocationFrom(loc: XYPair): XYPair = {
    this.direction match {
      case '>' => loc.>
      case '<' => loc.<
      case '^' => loc.^
      case 'v' => loc.v
    }
  }

  def break: Cart = {
    this.copy(isWorking = false)
  }

  def withNextDirection(currentPoint: XYPair, nextPoint: XYPair, graph: Map[XYPair, Set[XYPair]]): Cart = {
    val neighborsOfNext = graph(nextPoint)
    // straight cases
    if (neighborsOfNext.contains(this.nextLocationFrom(nextPoint))) {
      this
    } else {
      // turning cases
      val nextNextPoint = (neighborsOfNext - currentPoint).head
      val nextDirection = if (nextNextPoint == nextPoint.^) {
        '^'
      } else if (nextNextPoint == nextPoint.v) {
        'v'
      } else if (nextNextPoint == nextPoint.<) {
        '<'
      } else {
        '>'
      }
      this.copy(direction = nextDirection)
    }
  }

  def intersectionAdjusted: Cart = {
    (this.intersectionNum % 3 match {
      case 0 => this.turnLeft
      case 1 => this
      case 2 => this.turnRight
    }).copy(intersectionNum = this.intersectionNum + 1)
  }

  def turnLeft: Cart = {
    val newDirection = this.direction match {
      case '>' => '^'
      case '<' => 'v'
      case '^' => '<'
      case 'v' => '>'
    }
    this.copy(direction = newDirection)
  }

  def turnRight: Cart = this.turnLeft.turnLeft.turnLeft
}

def printGraphConnectivity(graph: Map[XYPair, Set[XYPair]]): Unit = {
  for {
    next <- graph.toSeq.sortBy(_._1)
  } {
    println(next)
  }
}


def buildGraph(input: Iterable[String]): Map[XYPair, Set[XYPair]] = {
  // Could improve by reusing the point references if it matters with mem usage
  val pointsToType = (for {
    (nextLine, y) <- input.zipWithIndex
    (nextChar, x) <- nextLine.zipWithIndex.toSet
    if !nextChar.isWhitespace
  } yield {
    XYPair(x, y) -> nextChar
  }).toMap.withDefaultValue(' ')

  var result = Map.empty[XYPair, Set[XYPair]].withDefaultValue(Set.empty)

  for {
    (point, pointType) <- pointsToType.toList
  } {
    val neighbors = pointType match {
      case '/' =>
        val isDownRight = pointsToType(point.^) match {
          case ' ' => pointsToType(point.v) match {
            case '|' | '+' => true
            case _ => false
          }
          case other => other match {
            case '|' | '+' => false
            case _ => true
          }
        }
        
        if (isDownRight) {
          Set(point.v, point.>)
        } else {
          Set(point.<, point.^)
        }
      case '\\' =>
        // copy-pasta
        val isDownLeft = pointsToType(point.^) match {
          case ' ' => pointsToType(point.v) match {
            case '|' | '+' => true
            case _ => false
          }
          case other => other match {
            case '|' | '+' => false
            case _ => true
          }
        }

        if (isDownLeft) {
          Set(point.<, point.v)
        } else {
          Set(point.^, point.>)
        }
      case '|' => Set(point.^, point.v)
      case '-' => Set(point.<, point.>)
      case '+' => Set(point.^, point.v, point.<, point.>)
      case '>' | '<' | '^' | 'v' =>
        // since there are no carts next to other carts in initial settings
        // we can rely on putting their direction in when filling other points
        Set.empty
    }

    for {
      nextNeighbor <- neighbors
    } {
      val neighborNeighbors = result(nextNeighbor) + point
      result = result + (nextNeighbor -> neighborNeighbors)
    }

    val pointNeighbors = result(point) ++ neighbors
    result = result + (point -> pointNeighbors)
  }

  printGraphConnectivity(result)

  result
}

def printGraph(input: Iterable[String], carts: Map[XYPair, Cart]): Unit = {
  println("\n===================")
  // print x-axis header
  println("===01234567890")

  for {
    (nextLine, y) <- input.zipWithIndex
  } yield {
    val sb = StringBuilder.newBuilder
    sb.append(f"$y%03d")

    for {
      (nextChar, x) <- nextLine.zipWithIndex
    } {

      val loc = XYPair(x, y)
      val c = if (carts.contains(loc)) {
        val cart = carts(loc)
        if (cart.isWorking) {
          carts(loc).direction
        } else {
          'X'
        }
      } else {
        nextChar match {
          case '>' | '<' | '^' | 'v' =>
            // TODO: need to replace the original carts with track lines once the cart moves
            '*'
          case other => other
        }
      }

      sb.append(c)
    }

    println(sb.toString())
  }
}

def buildCarts(input: Iterable[String]): Map[XYPair, Cart] = {
  (for {
    (nextLine, y) <- input.zipWithIndex
    (nextChar, x) <- nextLine.zipWithIndex
    next <- nextChar match {
      case direction @ ('>' | '<' | '^' | 'v') => Some(XYPair(x, y) -> Cart(direction))
      case _ => None
    }
  } yield {
    next
  }).toMap
}

val graph = buildGraph(rawInput)
val inputCarts = buildCarts(rawInput)

def moveWorldOneStep(graph: Map[XYPair, Set[XYPair]], carts: Map[XYPair, Cart], maxBroken: Int): Map[XYPair, Cart] = {
  var nextCarts = carts
  for {
    loc <- carts.keySet.toList.sorted
    cart = carts(loc)
    // stop when one failed
    if nextCarts.values.count(_.isWorking) != maxBroken
  } {
    nextCarts = cart.move(loc, graph, nextCarts)
  }
  nextCarts
}

printGraph(rawInput, inputCarts)

var carts = inputCarts
while (carts.values.forall(_.isWorking)) {
//for (_ <- 0 to 20) {
  carts = moveWorldOneStep(graph, carts, 1)
  printGraph(rawInput, carts)
}

// Answer 1
val ans1 = carts.toSet.find(!_._2.isWorking).get._1
// 69,46

//printGraph(rawInput, inputCarts)

// Answer 2