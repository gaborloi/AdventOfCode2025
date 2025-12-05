import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source

class TestTask1Suite extends AnyFunSuite {
  test("task1_1 test file") {
    assert(Task1.solveProblem1(Source.fromResource("input_test_1.txt")) == 3)
  }

  test("task1_1 all file") {
    assert(Task1.solveProblem1(Source.fromResource("input_all_1.txt")) == 982)
  }

  test("task1_2 test file") {
    assert(Task1.solveProblem2(Source.fromResource("input_test_1.txt")) == 6)
  }

  test("task1_2 all file") {
    assert(Task1.solveProblem2(Source.fromResource("input_all_1.txt")) == 6595)
  }
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
