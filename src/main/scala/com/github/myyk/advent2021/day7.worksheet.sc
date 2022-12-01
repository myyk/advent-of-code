import com.github.myyk.advent2021._

val crabLocations = readInput(2021,7).head.split(",").map(_.toInt).toSeq

crabLocations.min
crabLocations.max


val fakeCrabLocations = Seq(16,1,2,0,4,2,7,1,2,14)

type FuelCalculation = Int => Int

def minFuelSpent(crabLocations: Iterable[Int], fuelFormula: FuelCalculation): Int = {
    // min and max location
    val min = crabLocations.min
    val max = crabLocations.max
    val fuelUsageForEachLocation = for (i <- min to max) yield {
        // for each crab calculate how much distance to i
        crabLocations.map(x => fuelFormula(Math.abs(x - i))).sum
    }
    fuelUsageForEachLocation.min
}

def fuelToMove1(moveBy: Int): Int = {
    moveBy
}


// calculate fuel usage for fake crab locations
assert(minFuelSpent(fakeCrabLocations, fuelToMove1) == 37)

// calculate fuel usage for crab locations
val fuelSpent = minFuelSpent(crabLocations, fuelToMove1)

val answer1 = fuelSpent
//347011


def fuelToMove2(moveBy: Int): Int = {
    moveBy * (moveBy+1) / 2
}

// recalculate with the new fuel formula
minFuelSpent(fakeCrabLocations, fuelToMove2)
assert(minFuelSpent(fakeCrabLocations, fuelToMove2) == 168)

val fuelSpent2 = minFuelSpent(crabLocations, fuelToMove2)

val answer2 = fuelSpent2
//98363777