import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.collection.immutable.Range

object Task4 extends Task {
  val id = 4
  val window: List[Int] = List(-1, 0, 1)
  def solveProblem1(file: BufferedSource): Int = {
    val mapOfRolls = Utils.fileToCharArray(file)
    val rLength = mapOfRolls.length
    val cLength = mapOfRolls(0).length
    Range(0, rLength).foldLeft(0) { (rcRow, r) =>
      Range(0, cLength).foldLeft(rcRow) { (rcCol, c) =>
        if (mapOfRolls(r)(c) == '@') {
          val atCount = window.foldLeft(0) { (wRCount, wr) =>
            window.foldLeft(wRCount) { (wCCount, wc) =>
              if (checkCord(r + wr,c + wc, rLength, cLength, r, c) && (mapOfRolls(r + wr)(c + wc) == '@'))
                wCCount + 1 else wCCount
            }
          }
          if (atCount < 4) rcCol + 1 else rcCol
        } else rcCol
      }
    }
  }

  def checkCord(r: Int, c: Int, rLength: Int, cLength: Int, rc: Int, cc: Int): Boolean =
    (r > -1) && (c > -1) && (r < rLength) && (c < cLength) && ((r != rc) || (c != cc))
  def solveProblem2(file: BufferedSource): Int = {
    val mapOfRolls = Utils.fileToCharArray(file)
    roundCheck(mapOfRolls, 0)
  }

  @tailrec
  def roundCheck(mapOfRolls: Array[Array[Char]], removedCount: Int): Int = {
    val removables = collectRolls(mapOfRolls)
    if (removables.isEmpty) removedCount else {
      removables.foreach { cord =>
        mapOfRolls(cord._1)(cord._2) = '.'
      }
      roundCheck(mapOfRolls, removedCount + removables.size)
    }
  }
  def collectRolls(mapOfRolls: Array[Array[Char]]): Set[(Int, Int)] = {
    val rLength = mapOfRolls.length
    val cLength = mapOfRolls(0).length
    Range(0, rLength).foldLeft(Set[(Int,Int)]()) { (rsRow, r) =>
      Range(0, cLength).foldLeft(rsRow) { (rsCol, c) =>
        if (mapOfRolls(r)(c) == '@') {
          val atCount = window.foldLeft(0) { (wRCount, wr) =>
            window.foldLeft(wRCount) { (wCCount, wc) =>
              if (checkCord(r + wr, c + wc, rLength, cLength, r, c) && (mapOfRolls(r + wr)(c + wc) == '@'))
                wCCount + 1 else wCCount
            }
          }
          if (atCount < 4) rsCol + Tuple2(r,c) else rsCol
        } else rsCol
      }
    }
  }
}



