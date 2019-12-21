package problems.list

import org.scalatest._

class P14_P15_DuplicateElementsSpec extends WordSpec with Matchers {
  "P14_DuplicateElements" should {
    "group consecutive duplicates in run length encoding" in {
      P14_P15_DuplicateElements.execute(3, List("a", "b", "c", "c", "d")) shouldEqual List(
        "a",
        "a",
        "a",
        "b",
        "b",
        "b",
        "c",
        "c",
        "c",
        "c",
        "c",
        "c",
        "d",
        "d",
        "d")
      P14_P15_DuplicateElements.execute(1, List("a", "a")) shouldEqual List("a", "a")
      P14_P15_DuplicateElements.execute(5, List("a")) shouldEqual List("a", "a", "a", "a", "a")
      P14_P15_DuplicateElements.execute(3, List()) shouldEqual List()
    }
  }
}
