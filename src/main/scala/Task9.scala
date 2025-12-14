import Utils.{Cord, fileToCharArray}

import scala.io.BufferedSource

object Task9 extends Task {
  val id = 9

  case class LCord(x: Long, y: Long) {
    def dist(c2: LCord): Long = math.abs((x-c2.x + 1)*(y-c2.y + 1))
  }

  def solveProblem1(file: BufferedSource): Long = {
    val cords = file.getLines().map { l =>
      l.split(',') match
        case Array(a, b) => LCord(a.toLong, b.toLong)
    }.toList
    calcDistances(cords).max
  }

  def calcDistances(cords: List[LCord]): List[Long] = {
    (for (i <- cords.indices; j <- Range(i + 1, cords.length)) yield cords(i).dist(cords(j))).toList
  }
  def solveProblem2(file: BufferedSource): Long = {
    2L
  }
}
