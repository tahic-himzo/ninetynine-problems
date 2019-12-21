package problems.list

import org.scalatest._

class P05_ReverseListSpec extends WordSpec with Matchers {
  "P05_ReverseList" should {
    "return reverse a list" in {
      P05_ReverseList.execute(List(1, 1, 2, 3, 5, 8)) shouldEqual List(8, 5, 3, 2, 1, 1)
      P05_ReverseList.execute(List(1, 2)) shouldEqual List(2, 1)
      P05_ReverseList.execute(List(1)) shouldEqual List(1)
      P05_ReverseList.execute(List()) shouldEqual List()
    }
  }
}
