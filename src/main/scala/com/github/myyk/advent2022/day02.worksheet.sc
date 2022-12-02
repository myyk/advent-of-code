import com.github.myyk._

val input = readInput(2022,2)

sealed trait Hand:
    val score: Int
    def winsAgainst: Hand
    def losesTo: Hand
case object Rock extends Hand {
    val score = 1
    def winsAgainst: Hand = Scissors
    def losesTo = Paper
}
case object Paper extends Hand {
    val score = 2
    def winsAgainst: Hand = Rock
    def losesTo = Scissors
}
case object Scissors extends Hand {
    val score = 3
    def winsAgainst: Hand = Paper
    def losesTo = Rock
}


// The score for a single round is the score for the shape you selected
// (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the
// outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
sealed trait Outcome:
    val score: Int
case object Win extends Outcome {
    val score = 6
}
case object Draw extends Outcome {
    val score = 3
}
case object Lose extends Outcome {
    val score = 0
}

def letterToHand(c: Char): Hand = {
    // A for Rock, B for Paper, and C for Scissors
    // response: X for Rock, Y for Paper, and Z for Scissors

    c match {
        case 'A' | 'X' => Rock
        case 'B' | 'Y' => Paper
        case 'C' | 'Z' => Scissors
    }
}

letterToHand('A')
letterToHand('X')

object Outcome:
    def apply(theirs: Hand, mine: Hand): Outcome = {
        if (theirs == mine) {
            Draw
        } else {
            val isWin = mine.winsAgainst == theirs

            if (isWin) Win else Lose
        }
    }

def scoreRound(theirs: Hand, mine: Hand): Int = {
    Outcome(theirs, mine).score + mine.score
}

def totalScore(rounds: Iterable[(Hand,Hand)]): Int = {
    val roundScores = for {
        (theirs, mine) <- rounds
    } yield {
        scoreRound(theirs, mine)
    }
    
    roundScores.sum
}

val pairs = for {
    pair <- input.map(_.split(" "))
    them = pair(0).charAt(0)
    second = pair(1).charAt(0)
} yield {
    (letterToHand(them), second)
}

val rounds = for {
    (them, mine) <- pairs
} yield {
    (them, letterToHand(mine))
}

val answer1 = totalScore(rounds)
11063

def letterToOutcome(c: Char): Outcome = {
    // X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win

    c match {
        case 'X' => Lose
        case 'Y' => Draw
        case 'Z' => Win
    }
}

def induceHand(them: Hand, outcome: Outcome): Hand = {
    outcome match {
        case Draw => them
        case Win => them.losesTo
        case Lose => them.winsAgainst
    }
}

val actualRounds = for {
    (them, outcome) <- pairs
} yield {
    (them, induceHand(them, letterToOutcome(outcome)))
}

val answer2 = totalScore(actualRounds)
// 10349