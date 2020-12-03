import com.github.myyk.advent2020._

val input = readInput(3)

val open = '.'
val tree = '#'

val treeGeography = for {
  next <- input
} yield {
  for {
    nextChar <- next
  } yield {
    nextChar == tree
  }
}

case class Slope(angleDown:Int, angleRight:Int)

def countTreesOnPath(slope:Slope):Int = {
  val width = treeGeography.head.length

  val path = for {
    i <- 0 until treeGeography.length by slope.angleDown
    nextRow = treeGeography(i)
    j = i/slope.angleDown * slope.angleRight % width
  } yield {
    nextRow(j)
  }
  path.count(_.booleanValue())
}

// Answer 1
val answer1 = countTreesOnPath(Slope(1,3))

val slopes = Seq(
  Slope(1,1),
  Slope(1,3),
  Slope(1,5),
  Slope(1,7),
  Slope(2,1),
)

// Answer 2
val treesOnPaths = slopes.map(countTreesOnPath(_))
val answer2 = treesOnPaths.product
//206576000