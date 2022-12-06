import com.github.myyk._

val input = readInput(2022,6).head.toCharArray().toSeq

// Finds the index after the first unique sliding window of the given size.
// ex: [1,1,2,3] for n=2, returns 3
def findIndexOfFirstUniqueSlidingOfSizeN[T](iterable: Iterable[T], n: Int): Int = {
    val start = iterable.sliding(n).zipWithIndex.find((a, i) => a.toSet.size == n)
    val (_, lastNonUniqueIndex) = start.toSeq.last

    lastNonUniqueIndex + n
}

findIndexOfFirstUniqueSlidingOfSizeN(List(1,1,2,3), 2)

// tests from question setup
val sizeOfSignalStart = 4
assert(findIndexOfFirstUniqueSlidingOfSizeN("mjqjpqmgbljsphdztnvjfqwrcgsmlb".toCharArray(), sizeOfSignalStart) == 7)
assert(findIndexOfFirstUniqueSlidingOfSizeN("bvwbjplbgvbhsrlpgdmjqwftvncz".toCharArray(), sizeOfSignalStart) == 5)
assert(findIndexOfFirstUniqueSlidingOfSizeN("nppdvjthqldpwncqszvftbrmjlhg".toCharArray(), sizeOfSignalStart) == 6)
assert(findIndexOfFirstUniqueSlidingOfSizeN("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toCharArray(), sizeOfSignalStart) == 10)
assert(findIndexOfFirstUniqueSlidingOfSizeN("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray(), sizeOfSignalStart) == 11)

val answer1 = findIndexOfFirstUniqueSlidingOfSizeN(input, sizeOfSignalStart)
// 1175

val sizeOfStartOfMessage = 14
assert(findIndexOfFirstUniqueSlidingOfSizeN("mjqjpqmgbljsphdztnvjfqwrcgsmlb".toCharArray(), sizeOfStartOfMessage) == 19)
assert(findIndexOfFirstUniqueSlidingOfSizeN("bvwbjplbgvbhsrlpgdmjqwftvncz".toCharArray(), sizeOfStartOfMessage) == 23)
assert(findIndexOfFirstUniqueSlidingOfSizeN("nppdvjthqldpwncqszvftbrmjlhg".toCharArray(), sizeOfStartOfMessage) == 23)
assert(findIndexOfFirstUniqueSlidingOfSizeN("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toCharArray(), sizeOfStartOfMessage) == 29)
assert(findIndexOfFirstUniqueSlidingOfSizeN("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray(), sizeOfStartOfMessage) == 26)

val answer2 = findIndexOfFirstUniqueSlidingOfSizeN(input, sizeOfStartOfMessage)
// 3217