import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.util.matching.Regex
object Task1 extends Task {
  val id = 1
  val turnPattern: Regex = """(.)(\d*)""".r
  def solveProblem1(file: BufferedSource): Int = {
    checkHitting(0, 50, file.getLines())
  }

  @tailrec
  def checkHitting(counter: Int, currentValue: Int, turns: Iterator[String]): Int = {
    val next = turns.next()
    val modValue = next match
      case turnPattern(d, n) => if (d == "L") currentValue - n.toInt else currentValue + n.toInt

    val nextValue = modValue % 100
    val nextCounter = if (nextValue == 0) counter + 1 else counter
//    println(s"$modValue, $nextCounter, $nextValue")
    if (turns.hasNext) checkHitting(nextCounter, nextValue, turns) else nextCounter
  }

  def solveProblem2(file: BufferedSource): Int = {
    checkZeroCrossing(0, 50, file.getLines())
  }

  @tailrec
  def checkZeroCrossing(counter: Int, currentValue: Int, turns: Iterator[String]): Int = {
    val next = turns.next()
    val modValue = next match
      case turnPattern(d, n) => if (d == "L") - n.toInt else n.toInt

    val (nextValue, nextCounter) = modValue match
      case x if x > 0 => ((currentValue + modValue) % 100, (currentValue + modValue) / 100 + counter)
      case x if x == 0 => (currentValue, counter)
      case _ => if ((currentValue + (modValue % 100)) > 0 ) {
        (currentValue + (modValue % 100), counter - modValue / 100)
      } else {
        if ((currentValue + (modValue % 100)) == 0) {
          (0, counter - modValue / 100 + 1)
        } else {
          if (currentValue == 0) {
            (100 + currentValue + (modValue % 100), counter - modValue / 100)
          } else {
            (100 + currentValue + (modValue % 100), counter + 1 - modValue / 100)
          }
        }
      }

//    println(s"$next, $modValue, $nextCounter, $nextValue")
    if (turns.hasNext) checkZeroCrossing(nextCounter, nextValue, turns) else nextCounter
  }
}
