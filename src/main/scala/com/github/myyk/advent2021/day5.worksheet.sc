import com.github.myyk.advent2021._

// WARNING: this code is mostly written by an AI, so it's not very readable. (it actually added that last bit about readability)

val input = com.github.myyk.readInput(2021,5)

// a point is a case class with an x and y coordinate
case class Point(x: Int, y: Int)

// a line is a case class with a start and end point
case class Line(start: Point, end: Point) {
    // is vertical returns true when the start and end points have the same x coordinate
    def isVertical: Boolean = start.x == end.x

    // is horizontal returns true when the start and end points have the same y coordinate
    def isHorizontal: Boolean = start.y == end.y

    // is diagonal returns true if the lines has a slope of 1 or -1
    def isDiagonal: Boolean = (start.x - end.x).abs == (start.y - end.y).abs

    // return points crossed by the line
    def points: Seq[Point] = {
        // if the line is vertical, return all points with the same x coordinate
        if (isVertical) {
            // switch min and max to get the correct order
            val (min, max) = if (start.y < end.y) (start, end) else (end, start)
            (min.y to max.y).map(y => Point(start.x, y))
        }
        // if the line is horizontal, return all points with the same y coordinate
        else if (isHorizontal) {
            // switch min and max to get the correct order
            val (min, max) = if (start.x < end.x) (start, end) else (end, start)
            (min.x to max.x).map(x => Point(x, start.y))
        }
        // if the line is diagonal, return all points on the line
        else if (isDiagonal) {
            // if x is increasing use increasing range, else use decreasing range
            val xRange = if (start.x < end.x) (start.x to end.x) else (start.x to end.x by -1)
            // if y is increasing use increasing range, else use decreasing range
            val yRange = if (start.y < end.y) (start.y to end.y) else (start.y to end.y by -1)
            // zip ranges and make points
            xRange.zip(yRange).map(p => Point(p._1, p._2))
        }
        // if the line is neither vertical nor horizontal nor diagonal, return an empty sequence
        else {
            Seq()
        }
    }
}


// test a vertical line
val verticalLine = Line(Point(1,1), Point(1,3))
assert(verticalLine.isVertical)
assert(verticalLine.points == (Seq(Point(1,1), Point(1,2), Point(1,3))))

// test a horizontal line
val horizontalLine = Line(Point(1,1), Point(3,1))
assert(horizontalLine.isHorizontal)
assert(horizontalLine.points == (Seq(Point(1,1), Point(2,1), Point(3,1))))

// test a diagonal line
val diagonalLine = Line(Point(1,1), Point(3,3))
assert(diagonalLine.isDiagonal)
assert(diagonalLine.points == (Seq(Point(1,1), Point(2,2), Point(3,3))))

// test a reversed diagonal line
val reversedDiagonalLine = Line(Point(3,1), Point(1,3))
assert(reversedDiagonalLine.isDiagonal)
reversedDiagonalLine.points
assert(reversedDiagonalLine.points == (Seq(Point(3,1), Point(2,2), Point(1,3))))

// test another diagonal line
val anotherDiagonalLine = Line(Point(1,3), Point(3,1))
assert(anotherDiagonalLine.isDiagonal)
assert(anotherDiagonalLine.points == (Seq(Point(1,3), Point(2,2), Point(3,1))))

// test the last type of diagonal line
val lastDiagonalLine = Line(Point(3,3), Point(1,1))
assert(lastDiagonalLine.isDiagonal)
assert(lastDiagonalLine.points == (Seq(Point(3,3), Point(2,2), Point(1,1))))

// parse the input into lines from a format like this 822,976 -> 822,117
def parseLine(line: String): Line = {
    val start = line.split(" -> ")(0).split(",").map(_.toInt)
    val end = line.split(" -> ")(1).split(",").map(_.toInt)
    Line(Point(start(0), start(1)), Point(end(0), end(1)))
}

// map input into lines
val lines = input.map(parseLine)

def doubleCrossings(lines: Seq[Line]): Int = {
    // create empty crossings map by grouping lines by their points
    val crossingsUngrouped = for {
        line <- lines
        point <- line.points
    } yield point

    val crossings = crossingsUngrouped.groupBy(identity).view.mapValues(_.size)

    // find the points that have been crossed at least twice
    crossings.values.filter(_ >= 2).size
}

// a test line
val testLine = Line(Point(1,1), Point(1,3))
// reversed line
val reversedLine = Line(Point(1,3), Point(1,1))

assert(doubleCrossings(Seq(testLine, testLine)) == 3)
assert(doubleCrossings(Seq(testLine, reversedLine)) == 3)
assert(doubleCrossings(Seq(diagonalLine, diagonalLine)) == 3)

/*
make test lines from these
    0,9 -> 5,9
    8,0 -> 0,8
    9,4 -> 3,4
    2,2 -> 2,1
    7,0 -> 7,4
    6,4 -> 2,0
    0,9 -> 2,9
    3,4 -> 1,4
    0,0 -> 8,8
    5,5 -> 8,2
*/
val testLines = Seq(
    Line(Point(0,9), Point(5,9)),
    Line(Point(8,0), Point(0,8)),
    Line(Point(9,4), Point(3,4)),
    Line(Point(2,2), Point(2,1)),
    Line(Point(7,0), Point(7,4)),
    Line(Point(6,4), Point(2,0)),
    Line(Point(0,9), Point(2,9)),
    Line(Point(3,4), Point(1,4)),
    Line(Point(0,0), Point(8,8)),
    Line(Point(5,5), Point(8,2))
)

// get only the horizontal and vertical lines
val horizontalAndVerticalTestLines = testLines.filter(line => line.isVertical || line.isHorizontal)

assert(doubleCrossings(horizontalAndVerticalTestLines) == 5)


val horizontalAndVerticalLines = lines.filter(line => line.isVertical || line.isHorizontal)

val answer1 = doubleCrossings(horizontalAndVerticalLines)

// find double crossings for all lines
val answer2 = doubleCrossings(lines)
