//val input = 18
//val input = 42
val input = 3999

val fuelSquareSize = 3
val fuelCellDimension = 300
val gridSerialNumber = input

val fuelCells = Array.fill(fuelCellDimension)(Array.fill[Long](300)(0))

for {
  x <- 0 until fuelCellDimension
  y <- 0 until fuelCellDimension
} {
  val rackID = x + 10
  val basePowerLevel = (rackID * y + gridSerialNumber) * rackID
  fuelCells(x)(y) = basePowerLevel%1000/100 - 5
}

def fuelAt(x: Int, y: Int): Unit = {
  println(s"Fuel at ($x,$y) = ${fuelCells(x)(y)}")
}

def maxPower(fuelSquareSize: Int): (Long, Int, Int) = {
  val powers = for {
    x <- 0 until fuelCellDimension - fuelSquareSize + 1
    y <- 0 until fuelCellDimension - fuelSquareSize + 1
  } yield {
    val power = for {
      ox <- 0 until fuelSquareSize
      oy <- 0 until fuelSquareSize
    } yield {
      fuelCells(x + ox)(y + oy)
    }
    (power.sum, x, y)
  }

 powers.max
}

val (maxPowerValue, maxPowerX, maxPowerY) = maxPower(3)

// Answer 1
val ans1 = s"$maxPowerX,$maxPowerY"
// 21,77

//val squarePowers = for {
//  squareSize <- 3 to 300
//} yield {
//  val power = maxPower(squareSize)
//  (power._1, power._2, power._3, squareSize)
//}
//
//val (_, maxPower2X, maxPower2Y, maxPower2Size) = squarePowers.max


//def fuelSquares(): Map[(Int, Int, Int), Long] = {
def fuelSquares(): (Int, Int, Int) = {
  var max = (Long.MinValue, -1, -1, -1)

  var sizeXYToPower = (for {
    x <- 0 until fuelCellDimension
    y <- 0 until fuelCellDimension
  } yield {
    (1,x,y) -> fuelCells(x)(y)
  }).toMap

  var sizeToRowPower = (for {
    x <- 0 until fuelCellDimension
    y <- 0 until fuelCellDimension
  } yield {
    (1, x, y) -> fuelCells(x)(y)
  }).toMap

  var sizeToColPower = (for {
    x <- 0 until fuelCellDimension
    y <- 0 until fuelCellDimension
  } yield {
    (1, x, y) -> fuelCells(x)(y)
  }).toMap

  for {
    squareSize <- 2 to 300
  } {
    println(s"squareSize = $squareSize, max = $max")
//    sizeXYToPower = sizeXYToPower.filter(kv => kv._1._1 == squareSize - 1)
//    sizeToRowPower = sizeToRowPower.filter(kv => kv._1._1 == squareSize - 1)
//    sizeToColPower = sizeToColPower.filter(kv => kv._1._1 == squareSize - 1)

    for {
      x <- 0 until fuelCellDimension - squareSize + 1
      y <- 0 until fuelCellDimension - squareSize + 1
    } {
      val rowPower = sizeToRowPower.getOrElse(key = (squareSize - 1, x, y + squareSize - 1), default =
        (0 until squareSize).foldRight(0L){case (offset, sum) => sum + fuelCells(x+offset)(y + squareSize - 1)}
      )
      val colPower = sizeToColPower.getOrElse(key = (squareSize - 1, x + squareSize - 1, y), default =
        (0 until squareSize).foldRight(0L){case (offset, sum) => sum + fuelCells(x + squareSize - 1)(y+offset)}
      )
      val newCorner = fuelCells(x + squareSize - 1)(y + squareSize - 1)

      val power = sizeXYToPower(squareSize - 1, x, y) + rowPower + colPower + newCorner

      val memKey = (squareSize, x, y)
      val oldMemKey = (squareSize-1, x, y)
      val oldRowKey = (squareSize-1, x, y + squareSize - 1)
      val oldColKey = (squareSize-1, x + squareSize - 1, y)

      sizeToRowPower = sizeToRowPower + ((squareSize, x, y + squareSize - 1) -> (rowPower + newCorner)) - oldRowKey
      sizeToColPower = sizeToColPower + ((squareSize, x + squareSize - 1, y) -> (colPower + newCorner)) - oldColKey
      sizeXYToPower = sizeXYToPower + (memKey -> power) - oldMemKey

      if (max._1 < power) {
        max = (power, squareSize, x, y)
      }
    }
  }

  (max._2, max._3, max._4)
}

val (maxPower2Size,maxPower2X,maxPower2Y) = fuelSquares()


// Answer 2
// Won't work for if max is in size 1 or 2, I think, would fix if it mattered
val ans2 = s"$maxPower2X,$maxPower2Y,$maxPower2Size"
// 224,222,27