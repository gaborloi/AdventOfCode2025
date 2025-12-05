import scala.io.BufferedSource

trait Task {
  val id: Int
  def solveProblem1(input: BufferedSource): AnyVal
  def solveProblem2(input: BufferedSource): AnyVal
}
