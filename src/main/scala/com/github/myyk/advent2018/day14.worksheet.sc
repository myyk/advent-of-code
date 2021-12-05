import com.github.myyk.advent2018._

//val fileSource = false
val fileSource = true
val rawInput = if (fileSource) {
  com.github.myyk.readInput(2018,14).head.toInt
} else {
//  9
//  18
//  2018
//  51589
//  01245 // won't work, can manually inspect this case
//  92510
  59414 // expected 2018
//  1011 // expected 28
}

def stringToShorts(str: String): Iterable[Short] = {
  for {
    next <- str
  } yield {
    next.asDigit.toShort
  }
}

val input = stringToShorts("37")
var recipes = input.toVector
var elf1 = 0
var elf2 = 1

println(recipes.mkString(" "))

def createRecipes(in: Vector[Short], elf1: Int, elf2: Int): (Vector[Short], Int, Int) = {
  val sum = in(elf1) + in(elf2)
  val nextInput = in ++ stringToShorts(sum.toString)
  val nextElf1 = (elf1 + in(elf1) + 1) % nextInput.size
  val nextElf2 = (elf2 + in(elf2) + 1) % nextInput.size

  (nextInput, nextElf1, nextElf2)
}

while (recipes.size < rawInput + 10) {
  val (x, y, z) = createRecipes(recipes, elf1, elf2)
  recipes = x
  elf1 = y
  elf2 = z
}

println(recipes.mkString(" "))

// Answer 1
val ans1 = recipes.slice(rawInput, rawInput + 10).mkString
// 6107101544

// reset inputs
recipes = input.toVector
elf1 = 0
elf2 = 1
val seq = rawInput.toString.map(_.asDigit.toShort)

while (recipes.size < seq.size+1 || (recipes.takeRight(seq.size) != seq && recipes.takeRight(seq.size+1).dropRight(1) != seq)) {
  val (x, y, z) = createRecipes(recipes, elf1, elf2)
  recipes = x
  elf1 = y
  elf2 = z
}

// Answer 2
val ans2 = if (recipes.takeRight(seq.size) == seq) {
  recipes.size - seq.size
} else {
  recipes.size - seq.size - 1
}
// 20291131
