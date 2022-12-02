import com.github.myyk._

val input = readInput(2021,11)

// map input to numbers
val octopuses = input.map(_.toSeq.map(_.toInt - 48))

// point is a tuple of x and y
case class Point(x: Int, y: Int)

// OctopusMap is a map of points to their octopus flash values
case class OctopusMap(octopuses: Map[Point, Int], val flashes: Long) {
    // this is kind of dumb and inefficient, but it works
    val width = octopuses.keys.map(_.x).max + 1
    val height = octopuses.keys.map(_.y).max + 1

    // neighbors is up to 8 points surrounding, but not including those out side of range
    def neighbors(point: Point): Set[Point] = {
        Set(
            Point(point.x - 1, point.y),
            Point(point.x + 1, point.y),
            Point(point.x, point.y - 1),
            Point(point.x, point.y + 1),
            Point(point.x - 1, point.y - 1),
            Point(point.x + 1, point.y - 1),
            Point(point.x - 1, point.y + 1),
            Point(point.x + 1, point.y + 1)
        ).filter(p => p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
    }

    // to string the map
    override def toString: String = {
        val sb = new StringBuilder
        sb.append(s"Flashes = $flashes\n")
        for (y <- 0 until height) {
            for (x <- 0 until width) {
                sb.append(octopuses(Point(x, y)))
            }
            sb.append("\n")
        }
        sb.toString()
    }

    // diff this with another OctopusMap
    def diff(other: OctopusMap): Map[Point, Int] = {
        // assert dimensions are the same
        assert(width == other.width)
        assert(height == other.height)
        octopuses.filter { case (point, value) => other.octopuses(point) != value }
    }
}

// locationToOctopusMap
def locationToOctopusMap(octopuses: Seq[Seq[Int]]): OctopusMap = {
    val xMax = octopuses.head.size
    val yMax = octopuses.size
    val points = for {
        x <- 0 until xMax
        y <- 0 until yMax
    } yield Point(x, y)
    OctopusMap(points.map(p => (p, octopuses(p.y)(p.x))).toMap, 0)
}

// input string to OctopusMap
def inputToOctopusMap(input: String, flashes: Int = 0): OctopusMap = {
    val octopuses = input.split("\n").map(_.trim).filter(_.nonEmpty).toSeq.map(_.toSeq.map(_.asDigit))
    locationToOctopusMap(octopuses).copy(flashes = flashes)
}

// medium sized input
val mediumInput = """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""

val smallInput = """
11111
19991
19191
19991
11111
"""

val largeOctopusFlashMap = locationToOctopusMap(octopuses)
val mediumOctopusFlashMap = inputToOctopusMap(mediumInput)
val smallOctopusFlashMap = inputToOctopusMap(smallInput)

// test small dimensions
assert(smallOctopusFlashMap.width == 5)
assert(smallOctopusFlashMap.height == 5)

// test neighbors
assert(OctopusMap(Map(Point(0, 0) -> 1), 0).neighbors(Point(0, 0)) == Set.empty)
// test small neighbors
smallOctopusFlashMap.neighbors(Point(0, 0)) 
assert(smallOctopusFlashMap.neighbors(Point(0, 0)) == Set(Point(0, 1), Point(1, 0), Point(1, 1)))


// step through octopuses
// - increase octopus energy values by 1
// - if octopus energy passes 9, flash it by increasing all neighbors
// - if any of those pass 9, flash them by increasing all neighbors, etc
@scala.annotation.tailrec
final def step(octopusFlashMap: OctopusMap, toFlash: Set[Point], flashed: Set[Point], increaseAll: Boolean = false): OctopusMap = {
    // with new flashes
    val withNewFlashes = octopusFlashMap.copy(flashes = octopusFlashMap.flashes + toFlash.size)

    // increase if increaseAll is set
    val newOctopusFlashMap = if (increaseAll) {
        OctopusMap(withNewFlashes.octopuses.map {
            case (p, v) => (p, v + 1)
        }, withNewFlashes.flashes)
    } else {
        withNewFlashes
    }

    // get all neighbors of toFlash
    val neighbors = toFlash.toList.flatMap(newOctopusFlashMap.neighbors(_))

    // fold over neighbors - flash all octopuses
    val resultOctopusMap = neighbors.foldLeft(newOctopusFlashMap) { case (acc, p) =>
        val newValue = acc.octopuses(p) + 1
        OctopusMap(acc.octopuses.updated(p, newValue), acc.flashes)
    }

    // to flash neighbors next
    val flashableOctopusPoints = resultOctopusMap.octopuses.filter {
        case (_, v) => v > 9
    }.keySet

    val newToFlash = flashableOctopusPoints -- flashed 
    val newFlashed = flashableOctopusPoints ++ flashed

    if (newToFlash.isEmpty) {
        // reset all flashed values to 0
        OctopusMap(resultOctopusMap.octopuses.map {
            case (p, v) => (p, if (flashed.contains(p)) 0 else v)
        }.toMap, resultOctopusMap.flashes)
    } else {
        step(resultOctopusMap, newToFlash, newFlashed)
    }
}

// step 1 smallOctopusFlashMap
val smallStep1 = step(smallOctopusFlashMap, Set.empty, Set.empty, increaseAll = true)
// test that the dimensions didn't change
assert(smallStep1.width == smallOctopusFlashMap.width)
assert(smallStep1.height == smallOctopusFlashMap.height)
assert(smallStep1 == inputToOctopusMap(
"""
34543
40004
50005
40004
34543
""", 9))

// step N times
@scala.annotation.tailrec
final def stepN(octopusFlashMap: OctopusMap, n: Int): OctopusMap = {
    if (n == 0) {
        octopusFlashMap
    } else {
        stepN(step(octopusFlashMap, Set.empty, Set.empty, increaseAll = true), n - 1)
    }
}

// step unti all octopuses are 0
@scala.annotation.tailrec
final def stepUntilAllZero(octopusFlashMap: OctopusMap, acc: Int = 0): Int = {
    if (octopusFlashMap.octopuses.values.forall(_ == 0)) {
        acc
    } else {
        val newOctopusFlashMap = step(octopusFlashMap, Set.empty, Set.empty, increaseAll = true)
        stepUntilAllZero(newOctopusFlashMap, acc+1)
    }
}

// step 2 smallOctopusFlashMap
val smallStep2 = stepN(smallOctopusFlashMap, 2)
assert(smallStep2 == inputToOctopusMap(
"""
45654
51115
61116
51115
45654
""", 9))

// step 1 mediumOctopusFlashMap
val mediumStep1 = stepN(mediumOctopusFlashMap, 1)
assert(mediumStep1 == inputToOctopusMap(
"""
6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637
"""
))

// step 100 mediumOctopusFlashMap
val mediumStep100 = stepN(mediumOctopusFlashMap, 100)
assert(mediumStep100 == inputToOctopusMap(
"""
0397666866
0749766918
0053976933
0004297822
0004229892
0053222877
0532222966
9322228966
7922286866
6789998766
""", 1656))

// step 100 largeOctopusFlashMap
val largeStep100 = stepN(largeOctopusFlashMap, 100)


val answer1 = largeStep100.flashes
// 1617

// test step until all zero for mediumOctopusFlashMap
val mediumStepUntilAllZero = stepUntilAllZero(mediumOctopusFlashMap)
assert(mediumStepUntilAllZero == 195)

val answer2 = stepUntilAllZero(largeOctopusFlashMap)
// 258