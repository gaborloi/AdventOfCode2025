import scala.io.BufferedSource

object Task8 {
  val id = 8

  case class Cord3D(x: Int,y: Int, z: Int) {
    def dist(c2: Cord3D): Long =
      (x-c2.x).toLong * (x-c2.x).toLong + (y-c2.y).toLong * (y-c2.y).toLong + (z-c2.z).toLong * (z-c2.z).toLong
  }

  case class CordDist(i: Int, j: Int, d: Long)

  def solveProblem1(file: BufferedSource, n: Int): Int = {
    val cords = file.getLines().map { l =>
      l.split(',') match
        case Array(x: String, y: String, z: String) => Cord3D(x.toInt, y.toInt, z.toInt)
    }.toList
    val cordDists = calcDistances(cords).sortBy( _.d)
    val cordMappings = findJunctions(n, cordDists)

    println(cordMappings.keySet.intersect(cordMappings.values.toSet).size)
    cordMappings.groupBy(_._2).map { case (v, m) => m.size + 1 }.toList.sorted.takeRight(3).product
  }

  def calcDistances(cords: List[Cord3D]): List[CordDist] = {
    (for (i<-cords.indices; j<-Range(i + 1, cords.length)) yield CordDist(i, j, cords(i).dist(cords(j)))).toList
  }
  //cordMapping:  i = orig, j = new
  def findJunctions(n: Int, dist: List[CordDist]): Map[Int, Int] = {
    dist.take(n).foldLeft(Map[Int, Int]()) { (cordMapping,cd) =>
      val updateValues = Set(cd.j, cd.i, cordMapping.getOrElse(cd.j, cd.i), cordMapping.getOrElse(cd.i, cd.i))
      val minValue = updateValues.min
      val newKeys = Set(cd.j).union(updateValues.diff(Set(minValue))).diff(cordMapping.keySet)

      val cordMappingUpd = cordMapping.foldLeft(cordMapping) { case (cm, (k, v)) =>
        if (updateValues.contains(v)) cm.updated(k, minValue) else cm
      }
      newKeys.foldLeft(cordMappingUpd) { (cm, k) => cm.updated(k, minValue) }
    }
  }
  def solveProblem2(file: BufferedSource): Long = {
    2L
  }
}
