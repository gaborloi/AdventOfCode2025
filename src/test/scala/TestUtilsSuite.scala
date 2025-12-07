import org.scalatest.funsuite.AnyFunSuite


class TestUtilsSuite extends AnyFunSuite {

  test("Test Primes until 7") {
    assert(Utils.findPrimesUntil(7).diff(Set(2, 3, 5, 7)).isEmpty)
  }

  test("Test Primes until 2") {
    assert(Utils.findPrimesUntil(2).diff(Set(2)).isEmpty)
  }

  test("Test Primes until 1") {
    assert(Utils.findPrimesUntil(1).isEmpty)
  }
}