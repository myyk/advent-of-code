import com.github.myyk._

val input = readInput(2022,8)

val testInput = """
30373
25512
65332
33549
35390
""".split("\n").map(_.trim).filter(_.nonEmpty)

case class Location(col: Int, row: Int)

case class Tree(height: Short, isVisible: Boolean = false) {
    def hide: Tree = {
        copy(isVisible = false)
    }
    def show: Tree = {
        copy(isVisible = true)
    }
}

case class Forest(trees: Map[Location, Tree], rows: Int, cols: Int) {
    val visibleCount = trees.values.count(_.isVisible)
    val totalSize = rows*cols

    def rotatedClockWise: Forest = {
        val rotatedTrees = for {
            row <- (0 until rows)
            col <- (0 until cols)
        } yield {
            Location(rows-1-row, col) -> trees(Location(col, row))
        }
        Forest(trees=rotatedTrees.toMap, rows=cols, cols=rows)
    }

    def showTree(loc: Location): Forest = {
        copy(trees=trees.updatedWith[Tree](loc)(_.map(_.show)))
    }

    // bruteForce - see if this is good enough first
    def scenicScore(loc: Location): Int = {
        val height = trees(loc).height
        val lessThanThisTree: Location => Boolean = { loc =>
            trees(loc).height < height
        }

        val ranges = Seq(
            (0 until loc.row).reverse.map(Location(loc.col, _)), // up
            (0 until loc.col).reverse.map(Location(_, loc.row)), // left
            (loc.col+1 until cols).map(Location(_, loc.row)), // right
            (loc.row+1 until rows).map(Location(loc.col, _)), // down
        )

        if (ranges.exists(_.isEmpty)) {
            0
        } else {
            val scoreComponents = for {
                range <- ranges
            } yield {
                val numLessThan = range.takeWhile(lessThanThisTree).size
                // include the last tree compared, unless the while ended because it hit the wall
                numLessThan+1 min range.size
            }
            scoreComponents.product
        }
    }

    def scenicScores: Map[Location, Int] = {
        (allLocations zip allLocations.map(scenicScore)).toMap
    }

    private def allLocations: Iterable[Location] = {
        for {
            row <- (0 until rows)
            col <- (0 until cols)
        } yield {
            Location(col, row)
        }
    }

    override def toString(): String = {
        val rowStrings = for {
            row <- (0 until rows)
        } yield {
            val treeRow = for {
                col <- (0 until cols)
            } yield {
                trees(Location(col, row))
            }
            treeRow.map(_.height).mkString
        }
        return rowStrings.mkString("\n")
    }

    def visibilityMap: String = {
        val rowStrings = for {
            row <- (0 until rows)
        } yield {
            val visibilityOfTrees = for {
                col <- (0 until cols)
            } yield {
                if (trees(Location(col, row)).isVisible) {
                    "T"
                } else {
                    "F"
                }
            }
            visibilityOfTrees.mkString
        }
        return rowStrings.mkString("\n")
    }
}

object Forest {
    def apply(input: Iterable[String]): Forest = {
        val trees = for {
            (line, row) <- input.toVector.zipWithIndex
            (height, col) <- line.toCharArray.toVector.zipWithIndex
        } yield {
            Location(col, row) -> Tree(height = height.asDigit.toShort)
        }
        Forest(trees=trees.toMap, rows=input.size, cols=input.head.size)
    }
}

val testForest = Forest(testInput)
testForest.rotatedClockWise
testForest.visibilityMap

def observeColFromTop(forest: Forest, col: Int): Forest = {
    val (result, _) = (0 until forest.rows).foldLeft((forest, -1)) { case ((forest, tallest), row) =>
        val loc = Location(col, row)
        val tree = forest.trees(loc)
        if (tallest < tree.height) {
            (forest.showTree(loc), tree.height)
        } else (
            (forest, tallest)
        )
    }
    result
}

assert(observeColFromTop(testForest, 0).visibleCount == 2)
assert(observeColFromTop(testForest, 0).trees(Location(0,0)).isVisible)
assert(observeColFromTop(testForest.rotatedClockWise, 0).visibleCount == 3)

def observeColsFromTop(forest: Forest): Forest = {
    (0 until forest.cols).foldRight(forest){ (col, forest) =>
        observeColFromTop(forest, col)
    }
}

def determineVisible(forest: Forest): Forest = {
    // observe from all sides to see which trees you can see
    (1 to 4).foldRight(forest){ (_, forest) =>
        observeColsFromTop(forest.rotatedClockWise)
    }
}

val testVisible = determineVisible(testForest).visibleCount
assert(testVisible == 21)

val answer1 = determineVisible(Forest(input)).visibleCount
// 1843

Forest(input).totalSize

testForest.scenicScore(Location(2,1))
assert(testForest.scenicScore(Location(2,1)) == 4)
assert(testForest.scenicScore(Location(2,3)) == 8)
assert(testForest.scenicScores.values.max == 8)

val answer2 = Forest(input).scenicScores.values.max
// 180000 - this answer looks wrong, but it's surprisingly not