import com.github.myyk._

val input = readInput(2021,3).filter(_.nonEmpty)

// WARNING: this code is mostly written by an AI, so it's not very readable. (it actually added that last bit about readability)

// function to find mode of a position in a list of strings
def mode(list: Seq[String], pos: Int): Int = {
  list.map(_.charAt(pos).asDigit).groupBy(identity).maxBy(_._2.size)._1
}

// find mode of each of the 12 positions in the input
val modeOfPosition = for (i <- 0 to 11) yield {
    mode(input, i)
}

// reverse mode and multiply each mode by increasing powers of 2 and call that gammaRate
val gammaRate = modeOfPosition.reverse.zipWithIndex.map { case (mode, i) => mode * math.pow(2, i).toInt }.sum

// the bit inverse of gammarate masked with only the first 12-bits is the espilonRate
val espilonRate = ~gammaRate & 0xFFF

// multiply them together to get the power consumption
val powerConsumption = gammaRate * espilonRate

// this is answer 1
val answer1 = powerConsumption

// function that gives you the number of ones and zeros in a position of all strings
def countOnesAndZeros(list: Seq[String], pos: Int): (Int, Int) = {
  list.map(_.charAt(pos).asDigit).foldLeft((0, 0)) { case ((ones, zeros), digit) =>
    digit match {
      case 0 => (ones, zeros + 1)
      case 1 => (ones + 1, zeros)
    }
  }
}

// test that countOnesAndZeros works
assert(countOnesAndZeros(Seq("1", "1", "0"), 0) == (2, 1))

// function takes strings and a position then filters out strings that don't have the more common value in the positon
def filterByMostCommon(list: Seq[String], pos: Int): Seq[String] = {
    val (ones, zeros) = countOnesAndZeros(list, pos)
    // if there are equal numbers of ones and zeros, use the provided value
    if (ones >= zeros) {
        list.filter(_.charAt(pos).asDigit == 1)
    } else {
        list.filter(_.charAt(pos).asDigit == 0)
    }
}

// filter by least common
def filterByLeastCommon(list: Seq[String], pos: Int): Seq[String] = {
    val (ones, zeros) = countOnesAndZeros(list, pos)
    if (ones >= zeros) {
        list.filter(_.charAt(pos).asDigit == 0)
    } else {
        list.filter(_.charAt(pos).asDigit == 1)
    }
}

// test that filterByMode works
assert(filterByMostCommon(Seq("1", "1", "0", "0"), 0) == Seq("1", "1"))
assert(filterByMostCommon(Seq("1", "0", "0"), 0) == Seq("0", "0"))
assert(filterByLeastCommon(Seq("1", "1", "0", "0"), 0) == Seq("0", "0"))
assert(filterByLeastCommon(Seq("1", "0", "0"), 0) == Seq("1"))

var filteredInput = input
// continue filtering and increasing position
val filteredIterations = for (i <- 0 to 11) yield {
    // if there's more than one left
    if (filteredInput.size > 1) {
        // filter by mode
        filteredInput = filterByMostCommon(filteredInput, i)
    } else {
        filteredInput = Seq.empty
    }
    filteredInput
}

// find the first filteredIteration where the size is 1
val oxygenGeneratorRatingBinary = filteredIterations.find(_.size == 1).get.head
// convert binary to decimal
val oxygenGeneratorRating = Integer.parseInt(oxygenGeneratorRatingBinary, 2)

var filteredInput2 = input
// do same filtered Input iteratations again but with 0 instead of 1
val filteredIterations2 = for (i <- 0 to 11) yield {
    if (filteredInput2.size > 1) {
        filteredInput2 = filterByLeastCommon(filteredInput2, i)
    } else {
        filteredInput2 = Seq.empty
    }
    filteredInput2
}

val co2ScrubberRatingBinary = filteredIterations2.find(_.size == 1).get.head
val co2ScrubberRating = Integer.parseInt(co2ScrubberRatingBinary, 2)

// answer is when you multiply the two ratings
val answer2 = oxygenGeneratorRating * co2ScrubberRating
