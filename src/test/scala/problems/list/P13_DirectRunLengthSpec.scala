package problems.list

import org.scalatest._

class P13_DirectRunLengthSpec extends WordSpec with Matchers {
  "P13_DirectRunLength" should {
    "group consecutive duplicates in run length encoding" in {
      P13_DirectRunLength.execute(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) shouldEqual List(
        (4, "a"),
        (1, "b"),
        (2, "c"),
        (2, "a"),
        (1, "d"),
        (4, "e"))
      P13_DirectRunLength.execute(List("a", "a")) shouldEqual List((2, "a"))
      P13_DirectRunLength.execute(List("a", "b")) shouldEqual List((1, "a"), (1, "b"))
      P13_DirectRunLength.execute(List()) shouldEqual List()
    }
  }
}
