package problems.list

import org.scalatest._

class P07_FlattenListSpec extends WordSpec with Matchers {
  "P07_FlattenList" should {
    "flatten a list of lists" in {
      P07_FlattenList.execute(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1, 1, 2, 3, 5, 8)
      P07_FlattenList.execute(List(1, 2)) shouldEqual List(1, 2)
      P07_FlattenList.execute(List(1)) shouldEqual List(1)
      P07_FlattenList.execute(List(List(1))) shouldEqual List(1)
      P07_FlattenList.execute(List()) shouldEqual List()
    }
  }
}
