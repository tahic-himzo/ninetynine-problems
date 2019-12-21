package problems.list

import org.scalatest._

class P08_EliminateConsecDuplicatesSpec extends WordSpec with Matchers {
  "P08_EliminateConsecDuplicates" should {
    "remove consecutive duplicates" in {
      P08_EliminateConsecDuplicates.execute(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) shouldEqual List(
        "a",
        "b",
        "c",
        "a",
        "d",
        "e")
      P08_EliminateConsecDuplicates.execute(List("a", "a")) shouldEqual List("a")
      P08_EliminateConsecDuplicates.execute(List()) shouldEqual List()
    }
  }
}
