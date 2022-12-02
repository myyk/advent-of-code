import com.github.myyk._

val input = readInput(2020,6)

// TODO: same input grouping as Day4, move to a commmon function
var groupedInput = Vector.empty[String]

{
  var nextGroup = Vector.empty[String]
  for {
    nextLine <- input
  } yield {
    if (nextLine.isEmpty) {
      groupedInput = groupedInput :+ nextGroup.mkString(" ")
      nextGroup = Vector.empty[String]
    } else {
      nextGroup = nextGroup :+ nextLine
    }
  }
  if (nextGroup.nonEmpty) {
    groupedInput = groupedInput :+ nextGroup.mkString(" ")
  }
}

val groupYes1 = for {
  next <- groupedInput
} yield {
  next.replace(" ", "").toSet
}

// Answer 1
val answer1 = groupYes1.map(_.size).sum

val groupYes2 = for {
  next <- groupedInput
} yield {
  val groupAnswers = for {
    personAnswers <- next.split(" ")
  } yield {
    personAnswers.toSet
  }
  // Note: Part 1 was just union instead
  groupAnswers.reduce(_ intersect _)
}

// Answer 2
val answer2 = groupYes2.map(_.size).sum