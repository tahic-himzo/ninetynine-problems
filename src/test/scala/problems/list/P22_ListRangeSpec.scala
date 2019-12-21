package problems.list

import org.scalatest._

class P22_ListRangeSpec extends WordSpec with Matchers {
  "P22_ListRange" should {
    "create a list of ints between a given start and end (inclusive)" in {
      P22_ListRange.execute(3, 7) shouldEqual List(3, 4, 5, 6, 7)
      P22_ListRange.execute(-5, 2) shouldEqual List(-5, -4, -3, -2, -1, 0, 1, 2)
      P22_ListRange.execute(2, -5) shouldEqual List(2, 1, 0, -1, -2, -3, -4, -5)
      P22_ListRange.execute(0, 0) shouldEqual List(0)
    }
  }
}
