import com.github.myyk.advent2021._

// WARNING: this code is mostly written by an AI, so it's not very readable. (it actually added that last bit about readability)

val input = readInput(2021,4)

// the first line is the numbers called out in bingo
// next is a bunch of 5x5 bingo cards

// parse first line
val bingoNumbers = input.head.split(",").map(_.toInt).toSeq

// bingo cards contain a 2 dimensional grid of numbers and whether they have been marked or not
case class BingoCard(numbers: Seq[Seq[Int]], marked: Seq[Seq[Boolean]] = Seq.fill(5)(Seq.fill(5)(false))) {
    // mark number if it's contained in the numbers
    def mark(number: Int): BingoCard = {
        val row = numbers.indexWhere(_.contains(number))
        // the row has the number
        if (row >= 0) {
            val col = numbers(row).indexWhere(_ == number)
            // if the column has the number, mark it
            if (col >= 0) {
                val newMarked = marked.updated(row, marked(row).updated(col, true))
                BingoCard(numbers, newMarked)
            } else {
                this
            }
        } else {
            // the number is not in the row
            this
        }
    }

    // isBingo is there exists a row or column that is completely marked
    def isBingo: Boolean = {
        marked.exists(_.forall(_ == true)) || marked.transpose.exists(_.forall(_ == true))
    }

    // score is the sum of all unmarked numbers multiplied by last called number
    def score(lastCalled: Int): Int = {
        // zip numbers flattened with marks
        val zipped = numbers.flatten.zip(marked.flatten)
        // sum those and multipy by last called
        zipped.map(x => if (x._2) 0 else x._1).sum * lastCalled
    }
}

// test bingo card
assert(BingoCard(
    Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    ),
    Seq(
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false)
    )
).isBingo == false)
assert(BingoCard(
    Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    ),
    Seq(
        Seq(true, false, false, false, false),
        Seq(true, false, false, false, false),
        Seq(true, false, false, false, false),
        Seq(true, false, false, false, false),
        Seq(true, false, false, false, false)
    )
).isBingo == true)
assert(BingoCard(
    Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    ),
    Seq(
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(true, true, true, true, true),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false)
    )
).isBingo == true)

val card = BingoCard(
    Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    ),
    Seq(
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false)
    )
)

assert(card.mark(666) == card)
assert(card.mark(17) == BingoCard(
    Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    ),
    Seq(
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, false, false, false, false),
        Seq(false, true, false, false, false),
        Seq(false, false, false, false, false)
    )
))

assert(BingoCard(
    Seq(
        Seq(14, 21, 17, 24, 4),
        Seq(10 ,16 ,15,  9, 19),
        Seq(18 , 8 ,23, 26, 20),
        Seq(22 ,11 ,13,  6,  5),
        Seq(2  ,0 ,12 , 3 , 7)
    ),
    Seq(
        Seq(true, true, true, true, true),
        Seq(false, false, false, true, false),
        Seq(false, false, true, false, false),
        Seq(false, true, false, false, true),
        Seq(true, true, false, false, true)
    )
).score(24) == 4512)

val bingoCardsInputLines = input.tail.tail
// take 6 lines at a time
val bingoCardsInput = bingoCardsInputLines.grouped(6)

def parseBingoCard(input: Seq[String]): BingoCard = {
    // filter out the empty string
    val numbers = input.filter(_.nonEmpty).map(_.split(" ").filter(_.nonEmpty).map(_.toInt).toSeq)
    BingoCard(numbers)
}

// map the input to bingo cards
val bingoCards = bingoCardsInput.map(parseBingoCard).toSeq

// play bingo
def playBingoUntilNWins(cards: Seq[BingoCard], numbers: Seq[Int], wins: Int = 1): (BingoCard, Int) = {
    // for each number, mark the card
    // if a card is bingo, return it and the number
    // else continue to the next number
    var markedCards = cards
    var cardsRemoved = 0

    import scala.util.control.NonLocalReturns._
    returning {
        for (number <- numbers) {
            markedCards = markedCards.map(_.mark(number))
            markedCards.filter(_.isBingo).map { bingoCard =>
                // remove card
                markedCards = markedCards.filterNot(bingoCard.equals)
                cardsRemoved += 1
                if (cardsRemoved == wins) {
                    throwReturn((bingoCard, number))
                }
            }
        }
        // not possible to get here
        throwReturn (BingoCard(Seq.empty), -1)
    }
}

val (winningCard, winningNumber) = playBingoUntilNWins(bingoCards, bingoNumbers, 1)

val answer1 = winningCard.score(winningNumber)


// play bingo until the last winning card is bingo
val (losingCard, losingNumber) = playBingoUntilNWins(bingoCards, bingoNumbers, bingoCards.length)

val answer2 = losingCard.score(losingNumber)
