import com.github.myyk._

val input = readInput(2022,1)

val elvesToCarrying = for {
    elf <- groupNonEmptyLines(input)
} yield {
    elf.map(_.toInt)
}

val answer1 = elvesToCarrying.map(_.sum).max
// 72511

val answer2 = elvesToCarrying.map(_.sum).sorted.takeRight(3).sum
// 212117