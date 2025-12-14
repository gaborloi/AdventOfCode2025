import scala.annotation.tailrec
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
    val cordMappings = findJunctions(cordDists.take(n), Map[Int, Int]())

    cordMappings.groupBy(_._2).map { case (v, m) => m.size + 1 }.toList.sorted.takeRight(3).product
  }

  def calcDistances(cords: List[Cord3D]): List[CordDist] = {
    (for (i<-cords.indices; j<-Range(i + 1, cords.length)) yield CordDist(i, j, cords(i).dist(cords(j)))).toList
  }
  //cordMapping:  i = orig, j = new
  def findJunctions(dists: List[CordDist], initMap: Map[Int, Int]): Map[Int, Int] = {
    dists.foldLeft(initMap) { (cordMapping,cd) =>
      val updateValues = Set(cd.j, cd.i, cordMapping.getOrElse(cd.j, cd.i), cordMapping.getOrElse(cd.i, cd.i))
      val minValue = updateValues.min
      val newKeys = Set(cd.j).union(updateValues.diff(Set(minValue))).diff(cordMapping.keySet)

      val cordMappingUpd = cordMapping.foldLeft(cordMapping) { case (cm, (k, v)) =>
        if (updateValues.contains(v)) cm.updated(k, minValue) else cm
      }
      newKeys.foldLeft(cordMappingUpd) { (cm, k) => cm.updated(k, minValue) }
    }
  }
  def solveProblem2(file: BufferedSource, k: Int): Long = {
    val cords = file.getLines().map { l =>
      l.split(',') match
        case Array(x: String, y: String, z: String) => Cord3D(x.toInt, y.toInt, z.toInt)
    }.toList
    val cordDists = calcDistances(cords).sortBy(_.d)

    val lastCordDist = evalDists(cordDists,Map[Int,Int](), cords.size, k)
    cords(lastCordDist.i).x.toLong * cords(lastCordDist.j).x.toLong
  }

  @tailrec
  def evalDists(distList: List[CordDist], cordMappings: Map[Int, Int], cordCount: Int, k:Int): CordDist = {
    val cMUpd = findJunctions(distList.take(k), cordMappings)
    val eval = cMUpd.groupBy(_._2).size + cordCount - cMUpd.keySet.union(cMUpd.values.toSet).size
    if(eval == 1) {
      if (k == 1) distList.take(1).head else evalDists(distList, cordMappings, cordCount, math.max(k/10,1))
    } else evalDists(distList.drop(k), cMUpd, cordCount, k)
  }




}
