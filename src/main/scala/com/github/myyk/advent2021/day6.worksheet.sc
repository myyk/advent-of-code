import com.github.myyk.advent2021._

val input = com.github.myyk.readInput(2021,6).head
// split by commas into numbers
val numbers = input.split(",").map(_.toInt)


// lanternfish have an timer
case class LanternFish(timer: Int) {
    // reduce the timer and breed if 0
    def tick: Seq[LanternFish] = {
        if (timer > 0) {
            Seq(LanternFish(timer - 1))
        } else {
            breed
        }
    }
    
    // breed
    def breed: Seq[LanternFish] = Seq(LanternFish(6), LanternFish(8)) 
}

// import collection.immutable.MultiSet
type MultiSet[T] = Map[T, Long]

// make some fish from numbers
def makeFish(numbers: Iterable[Int]): MultiSet[LanternFish] = {
    val fish = numbers.map(LanternFish(_)->1L)
    fish.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
}

// run the simulation one tick
def runSimulationOneTick(lanternFishSet: MultiSet[LanternFish]): MultiSet[LanternFish] = {
    // get the next generation
    val updates = for ((fish, occurrence) <- lanternFishSet) yield {
        val nextGeneration = fish.tick
        nextGeneration.map(nextFish => nextFish->occurrence)
    }

    // group the flattened updates and add the occurences
    updates.flatten.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
}

// run simulation for n ticks
def runSimulation(lanternFishSet: MultiSet[LanternFish], n: Int): MultiSet[LanternFish] = {
    var currentSet = lanternFishSet
    for (i <- 0 until n) {
        currentSet = runSimulationOneTick(currentSet)
    }
    currentSet
}

// function that counds the number of fish in the multiset
def countFish(lanternFishSet: MultiSet[LanternFish]): Long = {
    lanternFishSet.values.sum
}

val startingTestTimers = Seq(3,4,3,1,2)
// map to test lanternfish
val testLanternFish = makeFish(startingTestTimers)
// store fish in a multiset

// run simulation for 1 day
val oneDayTick = runSimulationOneTick(testLanternFish)
val expected = makeFish(Seq(2,3,2,0,1))
assert(oneDayTick == expected)
val oneDay = runSimulation(testLanternFish, 1)
assert(oneDay == expected)

// run simulation for 18 days
val testLanternFish18 = runSimulation(testLanternFish, 18)
val testLanternFishCount = countFish(testLanternFish18)
assert(testLanternFishCount==26)

// run simulation for 80 days
val testLanternFish80 = runSimulation(testLanternFish, 80)
val testLanternFishCount80 = countFish(testLanternFish80)
assert(testLanternFishCount80==5934)

// run simulation for 256 days
val testLanternFish256 = runSimulation(testLanternFish, 256)
val testLanternFishCount256 = countFish(testLanternFish256)
assert(testLanternFishCount256==26984457539L)

// make fish from input numbers
val inputLanternFish = makeFish(numbers)
// run simulation for 80 days
val inputLanternFish80 = runSimulation(inputLanternFish, 80)
val inputLanternFishCount80 = countFish(inputLanternFish80)

val answer1 = inputLanternFishCount80
//386640

// run simulation for 256 days
val inputLanternFish256 = runSimulation(inputLanternFish, 256)
val inputLanternFishCount256 = countFish(inputLanternFish256)

val answer2 = inputLanternFishCount256
//1733403626279