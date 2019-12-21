package problems.list

import org.scalatest._

class P28A_SortByLengthSpec extends WordSpec with Matchers {
  "P28A_SortByLength" should {
    "sort a list of lists by list length" in {
      P28A_SortByLength.execute(List(List(1, 2, 3), List(1, 5), List(1, 3, 4, 5), List(8))) shouldEqual List(
        List(1, 3, 4, 5),
        List(1, 2, 3),
        List(1, 5),
        List(8))
    }
  }
}
