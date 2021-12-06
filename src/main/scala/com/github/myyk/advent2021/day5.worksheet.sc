import com.github.myyk.advent2021._

val input = com.github.myyk.readInput(2021,5)

// a point is a case class with an x and y coordinate
case class Point(x: Int, y: Int)

// a line is a case class with a start and end point
case class Line(start: Point, end: Point) {
    // is vertical returns true when the start and end points have the same x coordinate
    def isVertical: Boolean = start.x == end.x

    // is horizontal returns true when the start and end points have the same y coordinate
    def isHorizontal: Boolean = start.y == end.y

    // return points crossed by the line
    def points: Seq[Point] = {
        // if the line is vertical, return all points with the same x coordinate
        if (isVertical) {
            (start.y to end.y).map(y => Point(start.x, y))
        }
        // if the line is horizontal, return all points with the same y coordinate
        else if (isHorizontal) {
            (start.x to end.x).map(x => Point(x, start.y))
        }
        // if the line is neither vertical nor horizontal, return an empty sequence
        else {
            Seq()
        }
    }
}

// test a vertical line
val verticalLine = Line(Point(1,1), Point(1,3))
assert(verticalLine.points == (Seq(Point(1,1), Point(1,2), Point(1,3))))

// test a horizontal line
val horizontalLine = Line(Point(1,1), Point(3,1))
assert(horizontalLine.points == (Seq(Point(1,1), Point(2,1), Point(3,1))))

// // parse the input into lines from a format like this 822,976 -> 822,117
// def parseLine(line: String): Line = {
//     val start = line.split(" -> ")(0).split(",").map(_.toInt)
//     val end = line.split(" -> ")(1).split(",").map(_.toInt)
//     Line(Point(start(0), start(1)), Point(end(0), end(1)))
// }

// // map input into lines
// val lines = input.map(parseLine)

// // find the max boundaries for the lines
// val maxX = lines.map(_.start.x).max
// val maxY = lines.map(_.start.y).max

// // create a grid of points
// val grid = (0 to maxX).flatMap(x => (0 to maxY).map(y => Point(x, y)))

// // create a map of grid points to count of times crossed
// val crossings = grid.map(p => (p, lines.map(_.points).flatten.count(_ == p))).toMap

// // find the points that have been crossed at least twice
// val doubleOrMoreCrossings = crossings.filter(_._2 >= 2)

// val answer1 = doubleOrMoreCrossings

// val answer2 = 2