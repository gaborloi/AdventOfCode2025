import scala.annotation.tailrec
import scala.io.BufferedSource

object Task5 extends Task {
  val id = 5
  def solveProblem1(file: BufferedSource): Int = {
    val (rangeIter, valueIter) = file.getLines().span(_.nonEmpty)

    val ranges = rangeIter.map { rs =>
      val rv = rs.split('-')
      (rv(0).toLong, rv(1).toLong)
    }.toList
    val values = valueIter.toList.drop(1)

    values.foldLeft(0) { (s, v) =>
      val vl = v.toLong
      if (ranges.forall { case (a,b) => (a > vl) || (b < vl)}) s else s + 1 }
  }

  def solveProblem2(file: BufferedSource): Long = {
    val rangeIter = file.getLines().takeWhile(_.nonEmpty)
    val ranges = rangeIter.map { rs =>
      val rv = rs.split('-')
      (rv(0).toLong, rv(1).toLong)
    }.toList.sortBy((a,_) => a)
    collapseRanges(ranges.iterator, 0L,0L,0L) - 1
  }

  @tailrec
  def collapseRanges(it: Iterator[(Long, Long)], currentMin: Long, currentMax:Long, sum:Long): Long = {
    it.next match
      case (largeMin, largeMax) if largeMin > currentMax =>
        if (it.hasNext) collapseRanges(it, largeMin, largeMax, sum + currentMax + 1 - currentMin)
        else sum + currentMax + 1 - currentMin
      case (_, smallMax) if smallMax < currentMax =>
        if (it.hasNext) collapseRanges(it, currentMin, currentMax, sum)
        else sum + currentMax + 1 - currentMin
      case (_, nextMax) if nextMax >= currentMax =>
        if (it.hasNext) collapseRanges(it, currentMin, nextMax, sum)
        else sum + nextMax + 1 - currentMin
  }
}
