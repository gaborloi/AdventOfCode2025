import scala.io.BufferedSource
import Utils.fileToCharArray
import Utils.Cord

object Task7 extends Task {
  val id = 7
  val PRISM = '^'
  val SCATTER: List[Cord] = List(Cord(0,-1), Cord(0,1))
  case class Beam(cord: Cord) {
    def fall(map: Array[Array[Char]], nextBeams: Set[Beam], activatedPrisms: Set[Cord]): (Set[Beam], Set[Cord]) = {
      val newCord = cord + Cord(1, 0)
      val (nextCord, activatedPrismsUpd) = checkCord(newCord, map, nextBeams.map(_.cord), activatedPrisms)
      (nextCord.map(Beam.apply), activatedPrismsUpd)
    }

    final def checkCord(
      cord: Cord,
      map: Array[Array[Char]],
      nextCords: Set[Cord],
      activatedPrisms: Set[Cord]
    ): (Set[Cord], Set[Cord]) =
      if (map(cord.r)(cord.c) == PRISM) {
        if (activatedPrisms.contains(cord)) (nextCords, activatedPrisms) else {
          val activatedPrismsUpd = activatedPrisms + cord
          SCATTER.map { dir =>
            val sCord = cord + dir
            if (sCord.valid(map.length, map(0).length))
              checkCord(sCord, map, nextCords, activatedPrismsUpd) else (nextCords, activatedPrismsUpd)
          }.reduce( (a,b) => (a._1.union(b._1), a._2.union(b._2)))
        }
      } else (nextCords + cord, activatedPrisms)
  }

  def solveProblem1(file: BufferedSource): Int = {
    val map = fileToCharArray(file)
    val start = Cord(0, map.head.indexOf('S'))
    val startBeam = Beam(start)
    val (finalBeams, activatedPrisms) = Range(0, map.length - 1).foldLeft((Set(startBeam), Set[Cord]())) {
      case ((sB, sP), _) => sB.foldLeft((Set[Beam](), sP)) { case ((sB2, sP2), cB) =>
        cB.fall(map, sB2, sP2)
      }
    }
    activatedPrisms.size
  }

  def solveProblem2(file: BufferedSource): Long = {
    2L
  }
}
