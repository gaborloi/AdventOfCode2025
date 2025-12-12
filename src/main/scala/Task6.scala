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
  
  def solveProblem2(file: BufferedSource): Int = {
    2
  }
}
