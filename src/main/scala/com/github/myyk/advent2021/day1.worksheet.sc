import com.github.myyk.advent2021._

val input = readInput(2021,1).map(_.toInt)

// count the number of times that the previous number is less than the next number in input
def countIncreasing(input: Seq[Int]): Int = {
  input.sliding(2).count(x => x.head < x.last)
}


val answer1 = countIncreasing(input)

val depths = input
// map depths to sums of sliding windows of size 3
val denoisedDepths = depths.sliding(3).map(x => x.sum).toSeq

// use the new denoisedDepths to count the number of times that the previous number is less than the next number in input
val answer2 = countIncreasing(denoisedDepths)

