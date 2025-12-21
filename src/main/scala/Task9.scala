import Utils.{Cord, fileToCharArray}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task9 extends Task {
  val id = 9
  val EPS = 0.000001

  case class LCord(x: Long, y: Long) {
    def dist(c2: LCord): Long = math.abs((x-c2.x + 1)*(y-c2.y + 1))
  }

  case class LCordDist(i: Int, j: Int, d: Long)

  case class Section(c1: LCord, c2: LCord) {
    val m: Double = (c2.y - c1.y).toDouble / (c2.x - c1.x).toDouble
    val c: Double = (c1.y * c2.x - c2.y * c1.x).toDouble / (c2.x - c1.x).toDouble
    def xRange(): (Long, Long) = (math.min( c1.x, c2.x), math.max(c1.x, c2.x))
    def yRange(): (Long, Long) = (math.min( c1.y, c2.y), math.max(c1.y, c2.y))
    def isOnSectionLine(cord: LCord): Boolean = math.abs(cord.y.toDouble - m*cord.x.toDouble - c) < EPS
    def eval(x: Long): Double = m*x.toDouble + c
    def hasTrueBisection(s: Section): Boolean = {
      if (s.m == this.m) false else {

        val boxX = (math.max(xRange()._1, s.xRange()._1), math.min(xRange()._2, s.xRange()._2))
        val boxY = (math.max(yRange()._1, s.yRange()._1), math.min(yRange()._2, s.yRange()._2))

        if ((boxX._2 < boxX._1) || (boxY._2 < boxY._1)) false else {
          val startThisY = this.eval(boxX._1)
          val startSY = s.eval(boxX._1)
          val endThisY = this.eval(boxX._2)
          val endSY = s.eval(boxX._2)
          println(s"$this, $s, $boxX, $boxY, ${(startThisY > startSY) == (endThisY < endSY)}")
          (startThisY > startSY) == (endThisY < endSY) //TODO: equals could be bad
        }
      }
    }
  }
  
  def solveProblem1(file: BufferedSource): Long = {
    val cords = file.getLines().map { l =>
      l.split(',') match
        case Array(a, b) => LCord(a.toLong, b.toLong)
    }.toList
    calcDistances(cords).map(_.d).max
  }

  def calcDistances(cords: List[LCord]): List[LCordDist] = {
    (for (i <- cords.indices; j <- Range(i + 1, cords.length)) yield LCordDist(i, j, cords(i).dist(cords(j)))).toList
  }
  def solveProblem2(file: BufferedSource): Long = {
    val cords = file.getLines().map { l =>
      l.split(',') match
        case Array(a, b) => LCord(a.toLong, b.toLong)
    }.toList

    val lastCordIndex = cords.length - 1

    val sections = for (i <- cords.indices) yield if (i < lastCordIndex)
      Section(cords(i), cords(i + 1)) else Section(cords(i), cords.head)
    val distancesSorted = calcDistances(cords).sortBy(_.d).reverseIterator

    checkDistances(distancesSorted, cords, sections.toList)
  }
  
  @tailrec
  def checkDistances(distIter: Iterator[LCordDist], cords: List[LCord], sections: List[Section]): Long = {
    val dist = distIter.next()

    if (checkContainment(dist, cords, sections)) {
      dist.d
    } else {
      if (distIter.hasNext) checkDistances(distIter, cords, sections) else -1L
    }
  }
  def checkContainment(dist: LCordDist, cords: List[LCord], sections: List[Section]): Boolean = {
    val c1 = cords(dist.i)
    val c2 = cords(dist.j)
    val cm = LCord((c1.x+c2.x)/2, (c1.y + c2.y)/2)
    val cmSection = Section(cm, LCord(cm.x, 0L))
    val sectionCutCount = sections.map { s => if(s.hasTrueBisection(cmSection)) 1 else 0 }.sum()
    println(s"$c1, $c2, $cm, $sectionCutCount")
    if (sectionCutCount % 2 == 0) false else {
      val borders = List(
        Section(c1, LCord(c1.x, c2.y)),
        Section(c1, LCord(c2.x, c1.y)),
        Section(c2, LCord(c1.x, c2.y)),
        Section(c2, LCord(c2.x, c1.y)),
      )
      borders.forall { b => sections.forall(!_.hasTrueBisection(b)) }
    }
  }
}
