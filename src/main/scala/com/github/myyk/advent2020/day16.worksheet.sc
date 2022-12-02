import com.github.myyk._

val input = com.github.myyk.readInput(2020,16)

val constraintPattern = "([\\w\\s]+): ([\\d]+)-([\\d]+) or ([\\d]+)-([\\d]+)".r

case class Interval(min:Int, max:Int) {
  def in(n: Int): Boolean = {
    min <= n && n<= max
  }
}
case class Constraint(name:String, low:Interval, high:Interval) {
  def valid(n: Int): Boolean = {
    low.in(n) || high.in(n)
  }
}

val constraints = for {
  next <- input.takeWhile(_.nonEmpty)
} yield {
  next match {
    case constraintPattern(name, low1, high1, low2, high2) =>
      Constraint(name, Interval(low1.toInt, high1.toInt), Interval(low2.toInt, high2.toInt))
  }
}

def parseTicket(unparsed: String): Seq[Int] = {
  unparsed.split(",").toIndexedSeq.map(_.toInt)
}

val emptyLines = input.zipWithIndex.filter(_._1.isEmpty)
val myTicket = parseTicket(input(emptyLines(0)._2+2))

val (_, otherTicketsUnparsed) = input.splitAt(emptyLines(1)._2+2)
val otherTickets = otherTicketsUnparsed.map(parseTicket)

val invalidFields = for {
  ticket <- otherTickets
  field <- ticket
  if !constraints.exists(_.valid(field))
} yield {
  field
}

// Answer 1
val answer1 = invalidFields.sum
//29019

val validOtherTickets = for {
  ticket <- otherTickets
  if ticket.forall(field => constraints.exists(_.valid(field)))
} yield {
  ticket
}

val validTickets = validOtherTickets :+ myTicket

// let's see if brute force is computable bc it's fast to program
// ok, that's too slow, but can use to to verify
def isValidOrdering(constraints: Seq[Constraint], tickets: Seq[Seq[Int]]): Boolean = {
  tickets.forall( ticket =>
    (ticket zip constraints).forall { case (field, constraint) =>
      constraint.valid(field)
    }
  )
}

// too slow brute force
//val constraintSeq = constraints.permutations.find(isValidOrdering(_, validTickets))

//oops don't need this... but using as inspiration for positionToConstraint
val constraintToValidPositions: Map[Constraint, Set[Int]]= {
  val entries = for {
    constraint <- constraints
  } yield {
    val tickets = validTickets.toSet
    val indexToField = tickets.map(_.zipWithIndex).flatten.groupBy(_._2)
    val constraintValidForIndexes = for {
      (index, fields) <- indexToField
      if fields.forall{case (field, _) => constraint.valid(field)}
    } yield {
      index
    }

    constraint -> constraintValidForIndexes.toSet
  }
  entries.toMap
}

val zeroIndexConstraints = for {
  (constraint, positions) <- constraintToValidPositions
  position <- positions
  if position == 0
} yield  {
  constraint
}

val positionToConstraint: Map[Int, Set[Constraint]]= {
  val tickets = validTickets.toSet
  val indexToField = tickets.map(_.zipWithIndex).flatten.groupBy(_._2)

  for {
    (index, fields) <- indexToField
  } yield {
    index -> constraints.toSet.filter{ constraint =>
      fields.forall{case (field, _) => constraint.valid(field)}
    }
  }
}

val ticketSize = validTickets.last.size

for {
  i <- 0 until ticketSize
} {
  println(s"$i -> ${positionToConstraint(i).map(_.name)}")
}


// I think this works, but it is not fast enough. Needs memoization, I think.
def findValidOrdering(usedConstraints: Set[Constraint], acc: Seq[Constraint]): Seq[Constraint] = {
  if (usedConstraints.size == ticketSize) {
    acc
  } else {
    val toUseConstraints = positionToConstraint.getOrElse(acc.size, Set.empty) -- usedConstraints
    if (toUseConstraints.isEmpty) {
      Nil
    } else {
      toUseConstraints.view.map{ constraint =>
        findValidOrdering(usedConstraints + constraint, acc :+ constraint)
      }.find(_.nonEmpty).getOrElse(Nil)
    }
  }
}

// Maybe I should try to improve by trying to use the constraints that match the least fields first.
def findValidOrdering2(usedConstraints: Set[Constraint], acc: Seq[Constraint]): Seq[Constraint] = {
  if (usedConstraints.size == ticketSize) {
    acc
  } else {
    val toUseConstraints = positionToConstraint.getOrElse(acc.size, Set.empty) -- usedConstraints
    if (toUseConstraints.isEmpty) {
      Nil
    } else {
      toUseConstraints.toList.sortBy(constraintToValidPositions(_).size).view.map{ constraint =>
        findValidOrdering2(usedConstraints + constraint, acc :+ constraint)
      }.find(_.nonEmpty).getOrElse(Nil)
    }
  }
}

val positionsOrderedByLeastFrequent = positionToConstraint.toVector.sortBy(_._2.size).map(_._1)

// Make it a bit faster by also trying the positions by least used as well.
def findValidOrdering3(usedConstraints: Set[Constraint], acc: Map[Int,Constraint]): Seq[Constraint] = {
  if (usedConstraints.size == ticketSize) {
    acc.toSeq.sortBy(_._1).map(_._2)
  } else {
    val nextPosition = positionsOrderedByLeastFrequent(acc.size)
    val toUseConstraints = positionToConstraint.getOrElse(nextPosition, Set.empty) -- usedConstraints
    if (toUseConstraints.isEmpty) {
      Nil
    } else {
      toUseConstraints.toList.sortBy(constraintToValidPositions(_).size).view.map{ constraint =>
        val entry = nextPosition->constraint
        val updatedAcc = acc + entry
        findValidOrdering3(usedConstraints + constraint, updatedAcc)
      }.find(_.nonEmpty).getOrElse(Nil)
    }
  }
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

val constraintSeq = time { findValidOrdering2(Set.empty, Nil) }
val constraintSeq2 = time { findValidOrdering3(Set.empty, Map.empty) }

def ticketProduct(prefix: String, constraints: Seq[Constraint], ticket: Seq[Int]): Long = {
  val prefixedFields = for {
    (constraint, field) <- constraints zip ticket
    if constraint.name.startsWith(prefix)
  } yield {
    field
  }
  prefixedFields.map(_.toLong).product
}

// Answer 2
val answer2 = ticketProduct("departure", constraintSeq, myTicket)
val answer2_2 = ticketProduct("departure", constraintSeq2, myTicket)
//517827547723