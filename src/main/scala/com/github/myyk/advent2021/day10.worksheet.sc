import simulacrum.op
import com.github.myyk.advent2021._

val input = com.github.myyk.readInput(2021,10).map(_.toList)

// If a chunk opens with (, it must close with ).
// If a chunk opens with [, it must close with ].
// If a chunk opens with {, it must close with }.
// If a chunk opens with <, it must close with >.

// close to open parenthesis map
val closeToOpen = Map(
  ')' -> '(',
  ']' -> '[',
  '}' -> '{',
  '>' -> '<'
)

// match parenthesis until it is a corrupted pairing, then return the first illegal character. Otherwise, return None.
@scala.annotation.tailrec
final def matchParenthesis(chunk: List[Char], unmatched: List[Char] = Nil): Option[Char] = {
    chunk match {
        case Nil => None
        case '(' :: tail => matchParenthesis(tail, ')' :: unmatched)
        case '[' :: tail => matchParenthesis(tail, ']' :: unmatched)
        case '{' :: tail => matchParenthesis(tail, '}' :: unmatched)
        case '<' :: tail => matchParenthesis(tail, '>' :: unmatched)

        case close :: tail => 
            // check if the close matches the open
            val expectedClose = unmatched.head
            if (expectedClose != close) {
                Option(close)
            } else {
                matchParenthesis(tail, unmatched.tail)
            }
    }
}

val testInputLegal = "([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], (((((((((())))))))))".split(",").map(_.trim.toList).toSeq

// test that all testInputLegal are true
testInputLegal.foreach(chunk => assert(matchParenthesis(chunk).isEmpty))

val testInputCorrupted = "(], {()()()>, (((()))}, <([]){()}[{}])".split(",").map(_.trim.toList).toSeq

// test that all testInputCorrupted are false
testInputCorrupted.foreach(chunk => assert(matchParenthesis(chunk).nonEmpty))

// test input
val testInput = """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
""".split("\n").map(_.trim.toList).filter(_.nonEmpty).toSeq

// corrupted
// {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
// [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
// [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
// [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
// <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

assert(matchParenthesis("{([(<{}[<>[]}>{[]{[(<()>".toList) == Some('}'))
assert(matchParenthesis("[[<[([]))<([[{}[[()]]]".toList) == Some(')'))
assert(matchParenthesis("[{[{({}]{}}([{[{{{}}([]".toList) == Some(']'))
assert(matchParenthesis("[<(<(<(<{}))><([]([]()".toList) == Some(')'))
assert(matchParenthesis("<{([([[(<>()){}]>(<<{{".toList) == Some('>'))

// find all corrupted chunks in testInput
val corruptedTestChunks = testInput.flatMap(chunk => matchParenthesis(chunk))
assert(corruptedTestChunks.size == 5)


// syntax error scoring map
// ): 3 points.
// ]: 57 points.
// }: 1197 points.
// >: 25137 points.
val corruptionScore = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)

// expected score for test input = 26397
assert(corruptedTestChunks.map(corruption => corruptionScore(corruption)).sum == 26397)

// find all corrupted chunks in input
val corruptedChunks = input.flatMap(chunk => matchParenthesis(chunk))
// get score for each corrupted chunk
val corruptedChunksScore = corruptedChunks.map(corruption => corruptionScore(corruption))
// sum up all scores
val answer1 = corruptedChunksScore.sum
// 399153

// incomplete score
val incompleteScore = Map(
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4
)

// score a incomplete chunk by adding the scores in increasing powers of 5
def incompleteScoring(incompleteChunk: List[Char], acc: Long = 0): Long = {
    incompleteChunk match {
        case Nil => acc
        case head :: tail => incompleteScoring(tail, acc*5 + incompleteScore(head)) 
    }
}

// }}]])})] - 288957 total points.
assert(incompleteScoring("}}]])})]".toList) == 288957)
// )}>]}) - 5566 total points.
assert(incompleteScoring(")}>]})".toList) == 5566)
// }}>}>)))) - 1480781 total points.
assert(incompleteScoring("}}>}>))))".toList) == 1480781)
// ]]}}]}]}> - 995444 total points.
assert(incompleteScoring("]]}}]}]}>".toList) == 995444)
// ])}> - 294 total points.
assert(incompleteScoring("])}>".toList) == 294)

// match parenthesis until it is a corrupted pairing, then return the open of the first illegal character. Otherwise, return list of unmatched closes.
// I'm not using the extra functionality but wanted to see what a unified answer looked like
@scala.annotation.tailrec
final def findUnmatchedParenthesis(chunk: List[Char], unmatched: List[Char] = Nil): List[Char] = {
    chunk match {
        case Nil => unmatched
        case '(' :: tail => findUnmatchedParenthesis(tail, ')' :: unmatched)
        case '[' :: tail => findUnmatchedParenthesis(tail, ']' :: unmatched)
        case '{' :: tail => findUnmatchedParenthesis(tail, '}' :: unmatched)
        case '<' :: tail => findUnmatchedParenthesis(tail, '>' :: unmatched)

        case close :: tail => 
            // check if the close matches the open
            val expectedClose = unmatched.head
            if (expectedClose != close) {
                List(closeToOpen(close))
            } else {
                findUnmatchedParenthesis(tail, unmatched.tail)
            }
    }
}

// [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
// [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
// (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
// {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
// <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

// find unmatched parenthesis in [({(<(())[]>[[{[]{<()<>>
assert(findUnmatchedParenthesis("[({(<(())[]>[[{[]{<()<>>".toList) == "}}]])})]".toList)
assert(findUnmatchedParenthesis("[(()[<>])]({[<{<<[]>>(".toList) == ")}>]})".toList)
assert(findUnmatchedParenthesis("(((({<>}<{<{<>}{[]{[]{}".toList) == "}}>}>))))".toList)
assert(findUnmatchedParenthesis("{<[[]]>}<{[{[{[]{()[[[]".toList) == "]]}}]}]}>".toList)
assert(findUnmatchedParenthesis("<{([{{}}[<[[[<>{}]]]>[]]".toList) == "])}>".toList)

// open parenthesis set
val openParenthesis = Set('(', '[', '{', '<')

// find all incomplete chunks in testInput
val incompleteTestChunks = testInput.map(chunk => findUnmatchedParenthesis(chunk)).filter(incomplete => (incomplete.toSet intersect openParenthesis).isEmpty)
// expect size to be 5
assert(incompleteTestChunks.size == 5, s"${incompleteTestChunks.size}")
// find all incomplete test chunk scores
val incompleteTestChunkScores = incompleteTestChunks.map(chunk => incompleteScoring(chunk))
// find the median
val testAnswer = incompleteTestChunkScores.sorted.apply(incompleteTestChunkScores.size/2)
// should be 288957
assert(testAnswer == 288957)

// do the same for input
val incompleteChunks = input.map(chunk => findUnmatchedParenthesis(chunk)).filter(incomplete => (incomplete.toSet intersect openParenthesis).isEmpty)
// find all incomplete chunk scores
val incompleteChunkScores = incompleteChunks.map(chunk => incompleteScoring(chunk))
// find the median
val answer2 = incompleteChunkScores.sorted.apply(incompleteChunkScores.size/2)
// 2995077699
