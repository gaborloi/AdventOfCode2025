
class TestTask1Suite extends AnyFunSuite {
    test("task1_1 test file") {
      assert(Task1.parseFile1(Source.fromResource("input_test_1.txt")) == 11)
    }

    test("task1_1 all file") {
      assert(Task1.parseFile1(Source.fromResource("input_all_1.txt")) == 1319616)
    }

    test("task1_2 test file") {
      assert(Task1.parseFile2(Source.fromResource("input_test_1.txt")) == 31)
    }

    test("task1_2 all file") {
      assert(Task1.parseFile2(Source.fromResource("input_all_1.txt")) == 1319616)
    }
  }
}
