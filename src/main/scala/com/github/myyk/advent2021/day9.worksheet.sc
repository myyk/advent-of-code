import com.github.myyk._

val input = readInput(2021,9)

// the input contains a 2 dimensional array of numbers
val heights = input.map(_.map(_.asDigit))

// a point is a tuple of x and y coordinates
type Point = (Int, Int)

case class HeightMap(heights: Map[Point, Int]) {
    lazy val maxX = heights.keys.map(_._1).max
    lazy val maxY = heights.keys.map(_._2).max

    def apply(point: Point): Int = {
        heights((point._1, point._2))
    }

    // get neightbours of a point
    def neighbours(point: Point): Set[Point] = {
        val all = Set(
            (point._1 - 1, point._2),
            (point._1 + 1, point._2),
            (point._1, point._2 - 1),
            (point._1, point._2 + 1)
        )

        // remove points that are outside the map
        val n = all.filter { case (x, y) =>
            x >= 0 && x <= maxX && y >= 0 && y <= maxY
        }
        // for each assert n is in the heights map
        n.map(p => assert(heights.contains(p), s"$p is not in the map"))
        n
    }

    // is low point if neighbors all smaller
    def isLowPoint(x: Int, y: Int): Boolean = {
        assert(heights.contains((x, y)), s"$x, $y is not in the map")

        val height = heights((x, y))
        neighbours((x, y)).forall { case (x, y) =>
            heights((x, y)) > height
        }
    }

    // low points filters the height map for all the low points
    def lowPoints: Map[Point, Int] = {
        for {
            (point, height) <- heights
            if isLowPoint(point._1, point._2)
        } yield point -> height
    }

    // find basin sizes
    def basinSizes: Map[Point,Int] = {
        // traverse all basins and count the size
        for {
            (point, _) <- this.lowPoints
        } yield {
            point -> traverseBasin(point).size
        }
    }

    // traverse basin
    def traverseBasin(point: Point): Set[Point] = {
        var visited = Set(point)
        val queue = collection.mutable.Queue(point)

        while (queue.nonEmpty) {
            val current = queue.dequeue()
            val height = this.heights(current)
            val neighbours = this.neighbours(current).filter { case (x, y) =>
                val neighbourHeight = this.heights((x, y))
                neighbourHeight > height && neighbourHeight < 9 && !visited.contains((x, y))
            }
            queue.enqueueAll(neighbours)
            visited ++= neighbours
        }

        visited
    }
}

// test height map
val testHeightMap = HeightMap(Map(
    (0, 0) -> 1, (1, 0) -> 2, (2, 0) -> 3,
    (0, 1) -> 4, (1, 1) -> 7, (2, 1) -> 6,
    (0, 2) -> 7, (1, 2) -> 8, (2, 2) -> 2
))

// test neighbors (0,0)
assert(testHeightMap.neighbours((0,0)) == Set((0,1), (1,0)))

// test neighbors (1,0)
assert(testHeightMap.neighbours((1,0)) == Set((0,0), (1,1), (2,0)))

// test neighbors (1,1)
assert(testHeightMap.neighbours((1,1)) == Set((0,1), (1,0), (1,2), (2,1)))

// test neightbors (2,2)
assert(testHeightMap.neighbours((2,2)) == Set((2,1), (1,2)))

// test low points
assert(testHeightMap.lowPoints == Map((0,0) -> 1, (2,2) -> 2))

// build height map
def buildHeightMap(heights: Iterable[Iterable[Int]]): HeightMap = {
    // convert to height map of (x,y) -> height
    val heightMap = heights.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.map { case (height, x) =>
            (x, y) -> height
        }
    }.toMap

    HeightMap(heightMap)
}

// build height map from test data:

val smallHeights = Seq(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678",
).map(_.map(_.asDigit))

// build height map from smallHeights
val smallHeightMap = buildHeightMap(smallHeights)

// risk level is 1 + the height of the low points
val smallRiskLevel = smallHeightMap.lowPoints.map { case ((x, y), height) =>
    height + 1
}.sum
assert(smallRiskLevel == 15)

val point = (0,0)
var visited = Set(point)
val queue = collection.mutable.Queue(point)


// test basin
smallHeightMap.traverseBasin((1,0))
assert(smallHeightMap.traverseBasin((1,0)) == Set((0,0), (1,0), (0,1)))

// test basin sizes
assert(smallHeightMap.basinSizes == Map((1,0)->3, (9,0)->9,  (2,2)->14,  (6,4)->9))

val heightMap = buildHeightMap(heights)

assert(heightMap((0,0)) == 9)
assert(heightMap((1,0)) == 8)
assert(heightMap((0,3)) == 7)

// risk level is 1 + the height of the low points
val riskLevel = heightMap.lowPoints.map { case ((x, y), height) =>
    height + 1
}.sum

val answer1 = riskLevel
// 631

// height map's biggest 3 basins
val biggestBasins = heightMap.basinSizes.values.toSeq.sorted.reverse.take(3)

val answer2 = biggestBasins.product
// 821560