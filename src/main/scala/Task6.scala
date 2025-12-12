import scala.annotation.tailrec
import scala.io.BufferedSource

object Task6 extends Task {
  val id = 6
  def solveProblem1(file: BufferedSource): Long = {
    val arrayList = file.getLines().map (_.split(' ').filter(!_.isBlank).map(_.strip)).toList
    Range(0, arrayList.head.length).foldLeft(0L) { (sum, i) =>
      arrayList.last(i) match
        case "+" => arrayList.dropRight(1).map(_(i).toLong).sum + sum
        case "*" => arrayList.dropRight(1).map(_(i).toLong).product + sum
    }
  }

  def solveProblem2(file: BufferedSource): Long = {
    val iterList = file.getLines().map(_.iterator).toList

    parseNum(iterList, 0, 0, ' ')
  }

  @tailrec
  def parseNum(iterList: List[Iterator[Char]], sumValue: Long, sumTotal: Long, transformation: Char): Long = {
    val values = iterList.map { it => if(it.hasNext) it.next() else ' ' }
    val currentStr = values.dropRight(1).foldLeft("") { (s, c) => s + c.toString }.strip
    val currentNum = if (currentStr.isEmpty) 0L else currentStr.toLong

    if(iterList.forall(!_.hasNext)) {
      val newSumValue = transformation match
        case '+' => sumValue + currentNum
        case '*' => sumValue * currentNum
      sumTotal + newSumValue
    } else {
      if (currentNum == 0L) parseNum(iterList, sumValue, sumTotal, transformation) else {
        if (values.last.isWhitespace) {
          val newSumValue = transformation match
            case '+' => sumValue + currentNum
            case '*' => sumValue * currentNum
          parseNum(iterList, newSumValue, sumTotal, transformation)
        } else {
          parseNum(iterList, currentNum, sumTotal + sumValue, values.last)
        }
      }
    }
  }
}
