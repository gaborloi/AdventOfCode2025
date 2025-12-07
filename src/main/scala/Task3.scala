import scala.annotation.tailrec
import scala.io.BufferedSource

object Task3 extends Task {
  val id = 3
  def solveProblem1(file: BufferedSource): Long = {
    val lines = file.getLines()

    lines.foldLeft(0L) { (sumJolts: Long, bank: String) =>
      sumJolts + parseBank(bank.iterator, List[Int](), 2)
    }
  }

  def solveProblem2(file: BufferedSource): Long = {
    val lines = file.getLines()

    lines.foldLeft(0L) { (sumJolts: Long, bank: String) =>
      sumJolts + parseBank(bank.iterator, List[Int](), 12)
    }
  }

  @tailrec
  def parseBank(bankIter: Iterator[Char], joltList: List[Int], joltLength: Int): Long = {
    val nextNum = bankIter.next().toString.toInt
    val nextJoltList = if (joltList.length >= joltLength) {
      val joltIndex = checkJoltList(joltList.iterator, 10, 0)
      joltIndex match
        case Some(i) => joltList.take(i-1) ++ joltList.drop(i) :+ nextNum
        case None => if (joltList.last < nextNum) joltList.dropRight(1) :+ nextNum else joltList
    } else joltList :+ nextNum
    if (bankIter.hasNext) parseBank(bankIter, nextJoltList, joltLength) else {
      nextJoltList.map(_.toLong).reduce((a,b) => a * 10L + b)
    }
  }

  @tailrec
  def checkJoltList(joltListIter: Iterator[Int], previousValue: Int, index:Int): Option[Int] = {
    val nextValue = joltListIter.next()
    if (previousValue < nextValue) Some(index) else {
      if(joltListIter.hasNext) checkJoltList(joltListIter, nextValue, index + 1) else None
    }
  }
}



