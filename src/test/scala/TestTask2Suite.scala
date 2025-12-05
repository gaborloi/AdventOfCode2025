import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source

class TestTask2Suite extends AnyFunSuite {
  val task: Task = Task2
  val taskNr = task.id.toString
  val taskname = "task" + taskNr + "_1 test file"
  test(taskname) {
    assert(task.solveProblem1(Source.fromResource(s"input_test_${task.id}.txt")) == 1227775554L)
  }

  test("task2_1 all file") {
    assert(task.solveProblem1(Source.fromResource(s"input_all_${task.id}.txt")) == 12599655151L)
  }

  test("task2_1 debug file") {
    assert(task.solveProblem1(Source.fromResource(s"input_debug_${task.id}.txt")) == 1949094713L)
  }
//
//  test(s"task${task.id}_2 test file") {
//    assert(Task1.solveProblem2(Source.fromResource(s"input_test_${task.id}.txt")) == 6)
//  }
//
//  test(s"task${task.id}_2 all file") {
//    assert(Task1.solveProblem2(Source.fromResource(s"input_all_${task.id}.txt")) == 6595)
//  }
}

//class TestTask1Suite extends TableDrivenPropertyChecks {
//  val task1TestTable = Table(
//    ("FileName", "", "ExpectedVaule"),
//    ("input_test_1.txt", 3),
//    ("input_all_1.txt", 982),
//    ("input_test_2.txt", 6),
//    ("input_all_2.txt", 616)
//  )
//
//  val task1Test = TestTasks(Task1)
//  test("check for Task1") {
//    forEvery(task1TestTable) { (str, expectedResult) =>
//      task1Test(str, false) shouldBe expectedResult
//    }
//  }
//}
