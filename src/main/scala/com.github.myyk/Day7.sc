import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val fileSource = true
//val fileSource = false
val rawInputs = if (fileSource) {
  readInput(7)
} else {
  List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  )
}.sorted

val entry = raw"Step (\w) must be finished before step (\w) can begin.".r

val inputs = for {
  entry(to, from) <- rawInputs
} yield {
  to.toCharArray.head -> from.toCharArray.head
}

var graph = Map.empty[Char, Set[Char]].withDefaultValue(Set.empty)
for {
  (to, from) <- inputs
} {
  graph = graph + (to -> (graph(to) + from))
}


val roots = graph.keySet -- graph.values.flatten.toSet

//val sortedRoots = mutable.PriorityQueue[Char]().reverse
//sortedRoots.enqueue(roots.toSeq:_*)
//@tailrec
//def topoSort(roots: mutable.PriorityQueue[Char], visited: Set[Char], graph: Map[Char, Set[Char]], result: List[Char]): List[Char] = {
//  if (roots.isEmpty) {
//    result.reverse
//  } else {
//    val next = roots.dequeue
//    val toVisit = graph(next) -- visited
//    roots.enqueue(toVisit.toSeq:_*)
//
//    topoSort(roots, visited ++ toVisit, graph - next, next :: result)
//  }
//}
//val dag = topoSort(sortedRoots, Set.empty, graph, Nil)

@tailrec
def topoSort(possibleRoots: Set[Char], graph: Map[Char, Set[Char]], result: List[Char]): List[Char] = {
  if (possibleRoots.isEmpty) {
    result.reverse
  } else {
    val roots = possibleRoots -- graph.values.flatten.toSet
    val next = roots.toList.sorted.head

    topoSort(possibleRoots - next ++ graph(next), graph-next, next :: result)
  }
}

val dag = topoSort(roots, graph, Nil)

// Answer 1
val ans1 = dag.mkString