import scala.annotation.tailrec
import scala.io.BufferedSource

object Task2 extends Task {
  val id = 2
  def solveProblem1(file: BufferedSource): Long = {
    val ranges = file.getLines().next().split(',')
    ranges.foldLeft(0L) { (sumOfTwins, rString) =>
      val (minValue, maxValue) = rString.split('-') match //this could be regex
        case Array(a, b) => (a.toLong, b.toLong)

      val minString = minValue.toString

      val initValue = if (minString.length % 2 == 0) {
        val firstString = minString.substring(0, minString.length / 2)
        val firstCandidate = (firstString + firstString).toLong
        if (firstCandidate >= minValue) firstCandidate else (firstString.toLong + 1).toString.repeat(2).toLong
      } else ("1" + "0".repeat(minString.length/2)).repeat(2).toLong
      
      if (initValue < maxValue) sumDuplicates(sumOfTwins, initValue, maxValue) else sumOfTwins
    }
  }

  def findNextValue(currentValue: Long, maxValue: Long): Option[Long] = {
    val currentString = currentValue.toString
    val twinString = (currentString.substring(0, currentString.length / 2).toLong + 1L).toString
    val candidate = (twinString + twinString).toLong
    if (candidate <= maxValue) Some(candidate) else None
  }

  @tailrec
  def sumDuplicates(sumValue: Long, currentNumber: Long, maxNumber: Long): Long = {
    println(currentNumber)
    val currentNumberString = currentNumber.toString
    val newSum = (if (currentNumberString.length % 2 == 0) {
      val twinString = currentNumberString.substring(0, currentNumberString.length / 2)
      (twinString + twinString).toLong
    } else 0L) + sumValue

    findNextValue(currentNumber, maxNumber) match
      case Some(x) => sumDuplicates(newSum, x, maxNumber)
      case None => newSum
  }

  def solveProblem2(file: BufferedSource): Int = {
    2
  }
}
