import com.github.myyk.advent2018._

//val fileSource = false
val fileSource = true
val rawInputs = if (fileSource) {
  val first = com.github.myyk.readInput(2018,8).head
  val splits = first.split(" ").toList
  splits.map{s => s.toInt}
} else {
  val nums = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ").toList
  nums.map{s => s.toInt}
}

val intInputs = rawInputs

case class Tree(children: List[Tree], data: List[Int])

def readTree(inputs: Iterator[Int]): Tree = {
  val numChildren = inputs.next()
  val numData = inputs.next()

  val children = for {
    _ <- (0 until numChildren).toList
  } yield {
    readTree(inputs)
  }

  val data = for {
    _ <- (0 until numData).toList
  } yield {
    inputs.next()
  }

  Tree(children, data)
}

val tree = readTree(intInputs.iterator)

def sum(tree: Tree): Int = {
  val childrenSums = for {
    next <- tree.children
  } yield {
    sum(next)
  }
  tree.data.sum + childrenSums.sum
}

// Answer 1
val ans1 = sum(tree)
// 42768

def sum2(tree: Tree): Int = {
  if (tree.children.isEmpty) {
    return tree.data.sum
  }

  val childrenValues = tree.children.map(c => sum2(c))
  val values = for {
    next <- tree.data
    nextIndex = next - 1
    if nextIndex >= 0 && nextIndex < tree.children.size
  } yield {
    childrenValues(nextIndex)
  }
  values.sum
}

// Answer 2
val ans2 = sum2(tree)
// 34348
