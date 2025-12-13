import scala.annotation.targetName
import scala.io.{BufferedSource, Source}


object Utils {
  def findPrimesUntil(i: Int): Set[Int] = {
    Range(2, i + 1).foldLeft(Set[Int]()) { (prevPrimes, candidate) =>
      if (prevPrimes.forall { p => candidate % p != 0 }) prevPrimes + candidate else prevPrimes
     }
  }

  def fileToCharArray(file: BufferedSource): Array[Array[Char]] = {
    val lines = file.getLines().toList
    (for {
      line <- lines
    } yield line.toCharArray).toArray
  }

  case class Cord(r: Int, c: Int) {
    @targetName("add")
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    @targetName("subtract")
    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    @targetName("multiply")
    def *(that: Cord): Int = r * that.r + c * that.c

    def valid(maxRowIdx: Int, maxColIdx: Int): Boolean = (r <= maxRowIdx) && (c <= maxColIdx) && (r > -1) && (c > -1)

    @targetName("multiply")
    def *(const: Int): Cord = Cord(r * const, c * const)

    def rotateRight(): Cord = Cord(c, -r)
  }
}