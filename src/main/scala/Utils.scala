import scala.io.Source


object Utils {
  def findPrimesUntil(i: Int): Set[Int] = {
    Range(2, i + 1).foldLeft(Set[Int]()) { (prevPrimes, candidate) =>
      if (prevPrimes.forall { p => candidate % p != 0 }) prevPrimes + candidate else prevPrimes
     }
  }
}