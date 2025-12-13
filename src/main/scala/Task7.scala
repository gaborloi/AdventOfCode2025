import scala.io.BufferedSource
import Utils.fileToCharArray
import Utils.Cord

object Task7 extends Task {
  val id = 7
  val PRISM = '^'
  val SCATTER: List[Cord] = List(Cord(0,-1), Cord(0,1))
  case class Beam(cord: Cord, weight: Long) {
    def fall(map: Array[Array[Char]], nextBeams: List[Beam], activePrisms: Set[Cord]): (List[Beam], Set[Cord]) = {
      val newCord = cord + Cord(1, 0)
      checkCord(newCord, weight, map, nextBeams, activePrisms)
    }
  }
  final def checkCord(
    cord: Cord,
    weight: Long,
    map: Array[Array[Char]],
    nextBeams: List[Beam],
    activePrisms: Set[Cord]
  ): (List[Beam], Set[Cord]) =
    if (map(cord.r)(cord.c) == PRISM) {
      val activePrismsUpd = activePrisms + cord
      val additions: List[Beam] = SCATTER.map { dir => Beam(cord + dir, weight) }
      val consolidatedList: List[Beam] = List(nextBeams, additions).reduce { (a, b) =>
        b.foldLeft(a) { (l, bb) =>
          val iOps = l.indexWhere(lb => bb.cord == lb.cord)
          iOps match
            case -1 => l :+ bb
            case i =>
              val newWeight = bb.weight + l(i).weight
              l.take(i) ++ l.drop(i + 1) :+ Beam(bb.cord, newWeight)
        }
      }
      (consolidatedList, activePrismsUpd)
    } else {
//      println(s"${nextBeams :+ Beam(cord, weight)}")
      (nextBeams :+ Beam(cord, weight), activePrisms)
    }

  def solveProblem1(file: BufferedSource): Int = {
    val map = fileToCharArray(file)
    val start = Cord(0, map.head.indexOf('S'))
    val startBeam = Beam(start, 1)
    val (finalBeams, activatedPrisms) = Range(0, map.length - 1).foldLeft((List(startBeam), Set[Cord]())) {
      case ((sB, sP), _) =>
        sB.foldLeft((List[Beam](), sP)) { case ((sB2, sP2), cB) =>
        cB.fall(map, sB2, sP2)
      }
    }
    activatedPrisms.size
  }

  def solveProblem2(file: BufferedSource): Long = {
    val map = fileToCharArray(file)
    val start = Cord(0, map.head.indexOf('S'))
    val startBeam = Beam(start, 1)
    val (finalBeams, activatedPrisms) = Range(0, map.length - 1).foldLeft((List(startBeam), Set[Cord]())) {
      case ((sB, sP), _) => sB.foldLeft((List[Beam](), sP)) { case ((sB2, sP2), cB) =>
        cB.fall(map, sB2, sP2)
      }
    }
    finalBeams.map(_.weight).sum
  }
}
