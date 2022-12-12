import com.github.myyk._

import scala.annotation.tailrec

val input = readInput(2022,12)

val testInput = readStringInput("""
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
""")

type HeightMap = Map[Point, Int]
type DistanceMap = Map[Point, Int]

case class Mountain(heightMap: HeightMap, climber: Point, goal: Point) {
    val xMax = Points.boundaries(heightMap.keySet)._2.x
    val yMax = Points.boundaries(heightMap.keySet)._2.y

    def shortestClimberToGoal: Int = {
        shortestPathToGoal(climber).get
    }

    def shortestFromAnyLowpointToGoal: Int = {
        val lowpoints = heightMap.filter(_._2 == 0).keySet
        lowpoints.map(shortestPathToGoal).flatten.min
    }

    def shortestPathToGoal(p: Point): Option[Int] = {
        val distanceToPoint = shortestRouteLength(Vector(p), goal, Map(p -> 0))
        distanceToPoint.get(goal)
    }

    @tailrec
    final def shortestRouteLength(toVisit: Vector[Point], end: Point, distanceToPoint: DistanceMap): DistanceMap = {
        if (toVisit.isEmpty) {
            return distanceToPoint
        } else {
            val next = toVisit.head
            val distanceTravelled = distanceToPoint(next)
            if (next == end) {
                // we found it!
                distanceToPoint
            } else {
                // check neighbors if not already visited
                val neighbors = next.adjacents
                val eligibleNeighbors = neighbors
                    .filter(p => heightMap.contains(p) && heightMap(p) <= heightMap(next) + 1)
                    .filter(!distanceToPoint.contains(_))
                
                // add neighbor distances
                val neighborDistances = eligibleNeighbors.map(_->(distanceTravelled+1))
                val nextDistanceToPoint = distanceToPoint ++ neighborDistances

                shortestRouteLength(toVisit.tail :++ eligibleNeighbors, end, nextDistanceToPoint)
            }
        }
    }
}

def inputToMountain(input: Seq[String]): Mountain = {
    val rawHeightMap = (for {
        (line, y) <- input.zipWithIndex
        (char, x) <- line.zipWithIndex
    } yield {
        (Point(x,y), char - 'a')
    }).toMap

    val climber = rawHeightMap.find((_, key) => key == 'S'-'a').get._1
    val goal = rawHeightMap.find((_, key) => key == 'E'-'a').get._1

    val heightMap = rawHeightMap + (climber -> ('a'-'a')) + (goal -> ('z'-'a'))

    Mountain(heightMap, climber, goal)
}

def visualizeDistanceMap(distanceMap: DistanceMap): String = {
    val (min, max) = Points.boundaries(distanceMap.keySet)

    val distanceMapWithDefault = distanceMap.withDefaultValue(99)

    (for {
        y <- min.y to max.y
    } yield {
        (for {
            x <- min.x to max.x
        } yield {
            f"${distanceMapWithDefault(Point(x,y))}%02d"
        }).mkString(" ")
    }).mkString("\n")
}

def visualizeHeightMap(heightMap: HeightMap): String = {
    val (min, max) = Points.boundaries(heightMap.keySet)

    (for {
        y <- min.y to max.y
    } yield {
        (for {
            x <- min.x to max.x
        } yield {
            f"${('a' +heightMap(Point(x,y))).toChar}"
        }).mkString("")
    }).mkString("\n")
}


val testMountain = inputToMountain(testInput)
assert(testMountain.heightMap(testMountain.climber) == 0)
assert(testMountain.heightMap(testMountain.goal) == 25)
assert(testMountain.xMax == 7)
assert(testMountain.yMax==4)

assert(testMountain.shortestClimberToGoal==31)
visualizeDistanceMap(testMountain.shortestRouteLength(Vector(testMountain.climber), testMountain.goal, Map(testMountain.climber -> 0)))
visualizeHeightMap(testMountain.heightMap)

val answer1 = inputToMountain(input).shortestClimberToGoal
// 520

val answer2 = inputToMountain(input).shortestFromAnyLowpointToGoal
// 508