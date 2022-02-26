import utest.*

object ProcessorTests extends TestSuite {
  val tests = Tests {
    test("sanity") {
      val two = 2
      assert(3 == process{1 + two})
    }
  }
}