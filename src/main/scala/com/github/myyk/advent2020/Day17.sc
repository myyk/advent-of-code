import com.github.myyk.advent2020._

val active = '#'
val inactive = '.'

val input = com.github.myyk.readInput(202017)

case class Coordinate(x:Int, y:Int, z: Int, h: Int) {
  def neighbors: Seq[Coordinate] = {
    for {
      xOffset <- -1 to 1
      yOffset <- -1 to 1
      zOffset <- -1 to 1
      hOffset <- -1 to 1
      if !(xOffset==0 && yOffset==0 && zOffset==0 && hOffset==0)
    } yield {
      this.copy(
        x = x + xOffset,
        y = y + yOffset,
        z = z + zOffset,
        h = h + hOffset,
      )
    }
  }
}

def printSpace(space: Seq[Coordinate]):Unit={
  val minX = space.map(_.x).min
  val maxX = space.map(_.x).max
  val minY = space.map(_.y).min
  val maxY = space.map(_.y).max
  val minZ = space.map(_.z).min
  val maxZ = space.map(_.z).max

  for {
    z <- minZ to maxZ
  } {
    println(s"z=$z")
    for {
      y <- minY to maxY
    } {
      val row = for {
        x <- minX to maxX
      } yield {
        // TODO: add priting for 4d
        if (space.contains(Coordinate(x,y,z,0))) {
          active
        } else {
          inactive
        }
      }
      println(row.mkString(""))
    }
    println("")
  }
}

val activeSpace = for {
  (row,y) <- input.zipWithIndex
  (elem,x) <- row.zipWithIndex
  if elem == '#'
} yield {
  Coordinate(x,y,0,0)
}

/*
During a cycle, all cubes simultaneously change their state according to the following rules:

If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
 */
// TODO: This should take neighbors (Coordinate -> Seq[Coordinate]) function to separate 3d and 4d
def step(activeSpace: Seq[Coordinate]): Seq[Coordinate] = {
  val stillActive = activeSpace.filter{ active =>
    val activeNeighbors = active.neighbors.filter(activeSpace.contains(_)).size
    activeNeighbors == 2 || activeNeighbors == 3
  }

  val inactiveNeighbors = for {
    nextActive <- activeSpace
    neighbor <- nextActive.neighbors
    if !activeSpace.contains(neighbor)
  } yield {
    neighbor
  }

  val newlyActivate = for {
    (coordinate, group) <- inactiveNeighbors.groupBy(coordinate => coordinate).toSeq
    if group.size == 3
  } yield {
    coordinate
  }

  stillActive ++ newlyActivate
}

//printSpace(activeSpace)
val after1 = step(activeSpace)
println(after1.size)
//printSpace(after1)
val after2 = step(step(activeSpace))
println(after2.size)
//printSpace(after2)

val after6 = (1 to 6).foldLeft(activeSpace){(prev, _)=> step(prev)}

// Answer 1
val answer1 = after6.size
//213

//TODO: Could model z by abusing the neighbors function for z=0 by returning duplicates of z=1 and summing z>1 layers twice

// Answer 2
val answer2 = after6.size
//1624