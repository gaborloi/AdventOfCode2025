import sun.awt.im.InputMethodJFrame

import scala.io.Source

class TestTasks(
  task: Task
) {
  def solveTask(inputFile: String, isFirst: Boolean): AnyVal = {
    if (isFirst) task.solveProblem1(Source.fromResource(inputFile)) else
      task.solveProblem2(Source.fromResource(inputFile))
  }
}
