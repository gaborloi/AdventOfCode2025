import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask2Suite extends AnyFunSuite {
  val task: Task = Task2
  val taskNr: String = task.id.toString
  val taskname: String = "task" + taskNr + "_1 test file"
  test(taskname) {
    assert(task.solveProblem1(Source.fromResource(s"input_test_${task.id}.txt")) == 1227775554L)
  }

  test("task2_1 all file") {
    assert(task.solveProblem1(Source.fromResource(s"input_all_${task.id}.txt")) == 12599655151L)
  }

  test("task2_1 debug file") {
    assert(task.solveProblem1(Source.fromResource(s"input_debug_${task.id}.txt")) == 89660571L)
  }

  test("task2_2 test file") {
    assert(Task2.solveProblem2(Source.fromResource(s"input_test_${task.id}.txt")) == 4174379265L)
  }

  test(s"task2_2 all file") {
    assert(task.solveProblem2(Source.fromResource(s"input_all_${task.id}.txt")) == 20942028255L)
  }
}
