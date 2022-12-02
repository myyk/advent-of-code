import com.github.myyk._

val input = com.github.myyk.readInput(2020,18)

sealed trait Token
sealed trait Operation extends Token {
  def calculate(total: Long, value: Long): Long
}
object Add extends Operation {
  def calculate(total: Long, value: Long): Long = {
    total + value
  }
}
object Multi extends Operation {
  def calculate(total: Long, value: Long): Long = {
    total * value
  }
}
object Open extends Token
object Close extends Token
case class Number(value: Int) extends Token

val formulae =  for {
  str <- input
} yield {
  for {
    token <- str.replaceAll("""\(""", "( ").replaceAll("""\)""", " )").split(' ')
  } yield {
    token match {
      case "(" => Open
      case ")" => Close
      case "+" => Add
      case "*" => Multi
      case num: String => Number(num.toInt)
    }
  }
}

def calculateWeirdMath1(tokens: Iterator[Token]): Long = {
  var total = 0L
  var op: Operation = Add

  import scala.util.control.NonLocalReturns._

  returning {
    for {
      token <- tokens
    } {
      token match {
        case nextOp: Operation =>
          op = nextOp
        case Number(value) =>
          total = op.calculate(total, value)
        case Close =>
          throwReturn(total)
        case Open =>
          total = op.calculate(total, calculateWeirdMath1(tokens))
      }
    }

    throwReturn(total)
  }
}

val calculations1 = for {
  formula <- formulae
} yield {
  calculateWeirdMath1(formula.iterator)
}

// Answer 1
val answer1 = calculations1.sum
//21993583522852

def calculateWeirdMath2(tokens: Iterator[Token]): Long = {
  var total = 0L
  var op: Operation = Add

//  println("")
  import scala.util.control.NonLocalReturns._

  returning {
    for {
      token <- tokens
    } {
      token match {
        case Add =>
          op = Add
        case Multi =>
          op = Multi
          val next = calculateWeirdMath2(tokens)
  //        println(s"Multi $next * $total = ${Multi.calculate(total, next)}")
          throwReturn(Multi.calculate(total, next))
        case Number(value) =>
  //        println(s"Add $value + $total = ${op.calculate(total, value)}")
          total = op.calculate(total, value)
        case Close =>
  //        println(s"Close $total")
          throwReturn(total) 
        case Open =>
          total = op.calculate(total, calculateWeirdMath2(tokens))
      }
    }

    //  println(s"running total = $total")
    throwReturn(total) 
  }
}

val calculations2 = for {
  formula <- formulae
} yield {
  calculateWeirdMath2(formula.iterator)
}

// Answer 2
val answer2 = calculations2.sum
//122438593522757
