import com.github.myyk.advent2020._

import scala.annotation.tailrec
import scala.collection.mutable

val input = com.github.myyk.readInput(20207)

// stored from bag to types it has to hold
val graph = mutable.Map.empty[String, mutable.Map[String, Int]]
// stored from bag to its parent bags
val parentGraph = mutable.Map.empty[String, Set[String]]

val noBags = "([ \\w]+) bags contain no other bags.".r
val someBags = "([ \\w]+) bags contain (.*).".r
val nBags = "(\\d+) ([ \\w]+) bag.*".r

for {
  next <- input
} {
  next match {
    case noBags(childName) => {
      graph.addOne(childName, mutable.Map.empty)
      val old = parentGraph.getOrElse(childName, Set.empty)
      parentGraph.put(childName, old)
    }
    case someBags(name, rest) => {
      val bags = mutable.Map.newBuilder[String, Int]
      for {
        bag <- rest.split(",")
      } {
        bag.trim match {
          case nBags(n, childName) => {
            val old = parentGraph.getOrElse(childName, Set.empty)
            parentGraph.put(childName, old + name)

            bags.addOne(childName -> n.toInt)
          }
        }
      }
      graph.addOne(name, bags.result())

    }
  }
}

val target = "shiny gold"

@tailrec
def findParents(targets: List[String], seen: Set[String]): Set[String]=  {
  if (targets.isEmpty) {
    seen
  } else {
    val next = targets.head
    val parents = parentGraph.getOrElse(next, Set.empty)

    findParents((parents diff seen).toList ::: targets.tail, seen union parents)
  }
}

val parentsOfTarget = findParents(List(target), Set.empty)

// Answer 1
val answer1 = parentsOfTarget.size
//172

def countBags(targets: List[String], counts: Map[String, Int]): Map[String, Int] =  {
  if (targets.isEmpty) {
    counts
  } else {
    val next = targets.head
    val children = graph(next)
    val unseen = children.keySet diff counts.keySet

    val updatedCounts = unseen.foldLeft(counts)((nextCount,nextUnseen) => countBags(List(nextUnseen),nextCount))
    val nextCount = 1 + children.foldLeft(0){case(total, (name,count)) => total + updatedCounts(name)*count}
    updatedCounts + (next -> nextCount)
  }
}

val bagsThatHoldNone = for {
  (name, children) <- graph.toMap
 if children.isEmpty
}yield {
  name -> 1
}
val counts = countBags(List(target), bagsThatHoldNone)

// Answer 2
val answer2 = counts(target) - 1
// 39645