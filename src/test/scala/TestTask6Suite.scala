import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask6Suite extends AnyFunSuite {
  val task: Task = Task6
  val taskNr: String = task.id.toString
  val tasknames: List[String] = List(
    "task" + taskNr + "_1 test file",
    "task" + taskNr + "_1 all file",
    "task" + taskNr + "_2 test file",
    "task" + taskNr + "_2 all file"
  )
  val outputs: List[AnyVal] = List(4277556L, 7644505810277L, 14L, 350684792662845L)
  test(tasknames.head) {
    assert(task.solveProblem1(Source.fromResource(s"input_test_${task.id}.txt")) == outputs.head)
  }

  test(tasknames(1)) {
    assert(task.solveProblem1(Source.fromResource(s"input_all_${task.id}.txt")) == outputs(1))
  }

//  test(tasknames(2)) {
//    assert(task.solveProblem2(Source.fromResource(s"input_test_${task.id}.txt")) == outputs(2))
//  }
//
//  test(tasknames(3)) {
//    assert(task.solveProblem2(Source.fromResource(s"input_all_${task.id}.txt")) == outputs(3))
//  }
}
