import scala.annotation.tailrec
import scala.io.BufferedSource

object Task2 extends Task {
  val id = 2
  def solveProblem1(file: BufferedSource): Long = {
    val ranges = file.getLines().next().split(',')
    ranges.foldLeft(0L) { (sumOfTwins, rString) =>
      val (minValue, maxValue) = rString.split('-') match //this could be regex
        case Array(a, b) => (a.toLong, b.toLong)

      calcForRepCount(2, minValue, maxValue, Set[Long]()).sum() + sumOfTwins
    }
  }

  def findNextValue(currentValue: Long, maxValue: Long, repcount: Int): Option[Long] = {
    val currentString = currentValue.toString
    val twinString = (currentString.substring(0, currentString.length / repcount).toLong + 1L).toString
    val candidate = twinString.repeat(repcount).toLong
    if (candidate <= maxValue) Some(candidate) else None
  }

  @tailrec
  def sumDuplicates(setValues: Set[Long], currentNumber: Long, maxNumber: Long, repcount: Int): Set[Long] = {
    println(currentNumber)
    val currentNumberString = currentNumber.toString
    val newSet = if (currentNumberString.length % repcount == 0) {
      val twinString = currentNumberString.substring(0, currentNumberString.length / repcount)
      setValues + twinString.repeat(repcount).toLong
    } else setValues
      
    findNextValue(currentNumber, maxNumber, repcount) match
      case Some(x) => sumDuplicates(newSet, x, maxNumber, repcount)
      case None => newSet
  }

  def solveProblem2(file: BufferedSource): Long = {
    val ranges = file.getLines().next().split(',')
    ranges.foldLeft(0L) { (sumOfRanges, rString) =>
      println(rString)
      val (minValue, maxValue) = rString.split('-') match //this could be regex
        case Array(a, b) => (a.toLong, b.toLong)
      Utils.findPrimesUntil(maxValue.toString.length).foldLeft(Set[Long]()) { (setTwins, p) =>
        println(s"prime: $p")
        calcForRepCount(p, minValue, maxValue, setTwins).union(setTwins)
      }.sum() + sumOfRanges 
    }
  }

  def calcForRepCount(repcount: Int, minValue: Long, maxValue: Long, setOfTwins: Set[Long]): Set[Long]  = {
    val minString = minValue.toString
    val initValue = if (minString.length % repcount == 0) {
      val firstString = minString.substring(0, minString.length / repcount)
      val firstCandidate = firstString.repeat(repcount).toLong
      if (firstCandidate >= minValue) firstCandidate else {
        (firstString.toLong + 1).toString.repeat(repcount).toLong
      }
    } else ("1" + "0".repeat(minString.length / repcount)).repeat(repcount).toLong

    if (initValue < maxValue) sumDuplicates(setOfTwins, initValue, maxValue, repcount) else setOfTwins
  }
}



