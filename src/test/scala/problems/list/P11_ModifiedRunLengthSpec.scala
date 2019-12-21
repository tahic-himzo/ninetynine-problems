package problems.list

import org.scalatest._

class P11_ModifiedRunLengthSpec extends WordSpec with Matchers {
  "P11_ModifiedRunLength" should {
    "group consecutive duplicates in run length encoding while keeping single items as-is" in {
      P11_ModifiedRunLength.execute(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) shouldEqual List(
        (4, "a"),
        "b",
        (2, "c"),
        (2, "a"),
        "d",
        (4, "e"))
      P11_ModifiedRunLength.execute(List("a", "a")) shouldEqual List((2, "a"))
      P11_ModifiedRunLength.execute(List("a", "b")) shouldEqual List("a", "b")
      P11_ModifiedRunLength.execute(List()) shouldEqual List()
    }
  }
}
