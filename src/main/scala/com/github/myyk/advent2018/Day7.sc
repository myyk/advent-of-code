import com.github.myyk.advent2018._

import scala.annotation.tailrec
import scala.collection.mutable

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

def makeGraph(): Map[Char, Set[Char]] = {
  var graph = Map.empty[Char, Set[Char]].withDefaultValue(Set.empty)
  for {
    (to, from) <- inputs
  } {
    graph = graph + (to -> (graph(to) + from))
  }
  graph
}

var graph = makeGraph()

val roots = graph.keySet -- graph.values.flatten.toSet

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
// PFKQWJSVUXEMNIHGTYDOZACRLB

val baseSeconds = if (fileSource) { 60 } else { 0 }
val numElves = if (fileSource) { 4 } else { 2 }

def secondsToComplete(task: Char): Int = {
  (task - 'A') + baseSeconds + 1
}

// priority queue of when next work is done
val workQueue = mutable.PriorityQueue[(Int, Char)]().reverse
var secondsElapsed = 0

var taskGraph = makeGraph()

// get initial unblockedTasks
var unblockedTasks = taskGraph.keySet -- taskGraph.values.flatten.toSet

def addToUnblockedTasks(possibleUnblockedTasks: Set[Char]): Set[Char] = {
  unblockedTasks ++ possibleUnblockedTasks -- taskGraph.values.flatten.toSet
}

def completeATask(): Unit = {
  val (currentTime, task) = workQueue.dequeue()
  secondsElapsed = currentTime

  val possiblyUnblocked = taskGraph(task)
  taskGraph = taskGraph - task
  unblockedTasks = addToUnblockedTasks(possiblyUnblocked)

  println(s"completing $task at ${secondsElapsed}s")
  println(s"unblockedTask = $unblockedTasks, possiblyUnblocked = $possiblyUnblocked")
}

while (taskGraph.nonEmpty || workQueue.nonEmpty || unblockedTasks.nonEmpty) {
  // Take an unblocked task
  if (unblockedTasks.nonEmpty) {
    val nextTask = unblockedTasks.toList.sorted.head

    // Allocate work to elf by adding to priority queue
    // if there's less than numElves in queue.
    if (workQueue.size < numElves) {
      workQueue.enqueue((secondsElapsed + secondsToComplete(nextTask), nextTask))
      unblockedTasks = unblockedTasks - nextTask
    } else {
      // otherwise, wait for an elf, by moving time forward so we can
      // assign work to an elf.
      completeATask()
    }
  } else {
    // if we can't take a task, then move time forward until a task
    // completes and check again.
    completeATask()
  }
}

// Answer 2
val ans2 = secondsElapsed
