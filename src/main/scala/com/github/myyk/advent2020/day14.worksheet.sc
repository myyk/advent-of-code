import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(2020,14)

val maskPattern = "mask = (.*+)".r
val storePattern = "mem([\\d]+) = ([\\d]+)".r

case class Mask(mask: String) {
  def apply(num:Long): Long = {
    var include:Long = 0
    var exclude:Long = 0

    for {
      (char, i) <- mask.reverse.zipWithIndex
    } {
      char match {
        case 'X' =>
        case '1' =>
          include |= 1L<<i
        case '0' =>
          exclude |= 1L<<i
      }
    }

    exclude = ~exclude

    val thirtySixBitMask = 0xFFFFFFFFFL
    num&exclude|include&thirtySixBitMask
  }

  def addresses(num:Long): Seq[Long] = {
    var include:Long = 0
    var exclude:Long = 0

    for {
      (char, i) <- mask.reverse.zipWithIndex
    } {
      char match {
        case '0' =>
        case '1' =>
          include |= 1L<<i
        case 'X' =>
          exclude |= 1L<<i
      }
    }

    exclude = ~exclude

    val maskedNum = (num|include)&exclude
    addressesUnmasks().map(_|maskedNum)
  }

  def addressesUnmasks(): Seq[Long] = {
    mask.reverse.zipWithIndex.foldLeft(Seq(0L)){ case (acc, (char, i)) =>
      if (char != 'X') {
        acc
      } else {
        val mask = 1L<<i
        acc ++ acc.map(_|mask)
      }
    }
  }
}

def decodeV1():Map[Long, Long] = {
  var currentMask: Mask = Mask("")
  var registry = Map.empty[Long, Long].withDefaultValue(0L)
  for {
    next <- input
  } {
    // remove '[' and ']' bc they are difficult to remove with the regex library
    val str = next.filterNot(_ == '[').filterNot(_ == ']').mkString
    str match {
      case maskPattern(mask) =>
        currentMask = Mask(mask)
      case storePattern(location, value) =>
        registry = registry + (location.toLong -> currentMask(value.toLong))
    }
  }
  registry
}


// Answer 1
val answer1 = decodeV1().values.sum
//13476250121721

def decodeV2():Map[Long, Long] = {
  var currentMask: Mask = Mask("")
  var registry = Map.empty[Long, Long].withDefaultValue(0L)
  for {
    next <- input
  } {
    // remove '[' and ']' bc they are difficult to remove with the regex library
    val str = next.filterNot(_ == '[').filterNot(_ == ']').mkString
    str match {
      case maskPattern(mask) =>
        currentMask = Mask(mask)
      case storePattern(location, value) =>
        for {
          address <- currentMask.addresses(location.toLong)
        } {
          registry = registry + (address -> value.toLong)
        }
    }
  }
  registry
}

// Answer 2
val answer2 = decodeV2().values.map(BigInt(_)).sum
//4463708436768
